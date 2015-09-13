{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
module Tandoori.Typing.Monad ( ClsInfo (..)
                             , VarSet
                             , Typing
                             , runTyping
                             , mkTv, mkTyVar
                             , askCtx, withCtx
                             , tellVar, stopVars
                             , listenVars, askSrc
                             , orRecover, addError
                             , askCon, askClass, askSupers, askInstance
                             , askUserDecl, askPolyVar, askForcedMonoVars
                             , withCons, withClasses, withInstances
                             , withUserDecls, withMonoVars --, withPolyVars
                             , raiseError, withLoc, withSrc, withLSrc) where

import           Control.Applicative
import           Control.Arrow           (first)
import           Control.Monad.Error
import           Control.Monad.RWS       (RWS, asks, censor, listen, local,
                                          runRWS, tell)
import           Data.Map                (Map)
import qualified Data.Map                as M
import           Data.Monoid
import           Data.Set                (Set)
import qualified Data.Set                as S
import           Tandoori.Aliases
import           Tandoori.GHC.Internals
import           Tandoori.Supply
import           Tandoori.Typing
import           Tandoori.Typing.Context
import           Tandoori.Typing.Error
import           Tandoori.Typing.MonoEnv

--- Reader
type KindMap = Map TvName Int -- TODO: DataName
type ConMap = Map ConName Ty
data ClsInfo = ClsInfo { clsSupers :: [Cls],
                         clsParam  :: Tv,
                         clsMeths  :: Map VarName (Located PolyTy) }
type ClsMap = Map Cls ClsInfo
type InstMap = Map (Cls, TyCon) PolyTy

data R = R { loc       :: SrcSpan,
             src       :: Maybe SDoc,

             kindMap   :: KindMap,
             conMap    :: ConMap,
             classMap  :: ClsMap,
             instances :: InstMap,

             ctx       :: Context }

--- Writer
type VarSet = Set VarName
type W = ([ErrorMessage], VarSet)

--- The Typing monad
newtype Typing a = Typing { unTyping :: ErrorT ErrorMessage (RWS R W (Supply Tv)) a}
                 deriving (Monad, Applicative, Functor)

runTyping :: Typing a -> (Maybe a, [ErrorMessage])
runTyping typing = case result of
                     Left err     -> (Nothing, err:output)
                     Right result -> (Just result, output)
  where
    (result, s', (output, _)) = (runRWS . runErrorT . unTyping) typing r s

    r = R { loc       = noSrcSpan
          , src       = Nothing
          , kindMap   = M.empty
          , conMap    = M.empty
          , classMap  = M.empty
          , instances = M.empty
          , ctx       = mkContext
          }

    s = Supply $ map toTv [1..]
      where
        toTv i = let name  = 't' : show i
                     uname = mkAlphaTyVarUnique i
                 in mkSysTvName uname (mkFastString name)

        typing' = do
          α <- mkTyVar
          let mapCon = map $ first dataConName
          let cons = mapCon [ (nilDataCon,   tyList α)
                            , (consDataCon,  tyCurryFun [α, tyList α, tyList α])
                            , (trueDataCon,  tyBool)
                            , (falseDataCon, tyBool)
                            ]
          withCons cons typing

mkTv :: Typing Tv
mkTv = Typing getSupply

mkTyVar :: Typing Ty
mkTyVar = TyVar <$> mkTv

withLoc :: SrcSpan -> Typing a -> Typing a
withLoc loc = Typing . local setLoc . unTyping
  where setLoc r = r{loc}

withSrc :: Outputable e => e -> Typing a -> Typing a
withSrc src = Typing . local setSrc . unTyping
  where setSrc r = r { src = Just $ ppr src }

askSrc :: Typing (Maybe SDoc)
askSrc = Typing $ asks src

withLSrc :: Outputable e => Located e -> Typing a -> Typing a
withLSrc (L loc src) = withLoc loc . withSrc src

mkErrorMsg err = do
  loc <- Typing $ asks loc
  src <- Typing $ asks src
  return $ ErrorMessage (ErrorLocation loc src) err

addError :: ErrorContent -> Typing ()
addError err = do
  msg <- mkErrorMsg err
  tellErrors [msg]

tellErrors :: [ErrorMessage] -> Typing ()
tellErrors msgs = Typing $ tell (msgs, mempty)

tellVar :: VarName -> Typing ()
tellVar var = tellVars $ S.singleton var

tellVars :: VarSet -> Typing ()
tellVars vars = Typing $ tell (mempty, vars)

stopVars :: Typing a -> Typing a
stopVars = Typing . censor (fmap (const mempty)) . unTyping

listenVars :: Typing a -> Typing (a, VarSet)
listenVars f = Typing $ do
  (result, (_, vars)) <- listen $ unTyping f
  return (result, vars)

raiseError :: ErrorContent -> Typing a
raiseError err = do
  msg <- mkErrorMsg err
  Typing $ throwError msg

orRecover :: Typing a -> Typing a -> Typing a
orRecover a b = Typing $ unTyping a `catchError` handler
  where
    handler err = do
      unTyping $ tellErrors [err]
      unTyping b

askCtx = Typing $ asks ctx

askForcedMonoVars = Typing $ asks $ monoVars . ctx

askCon name = do
  lookup <- Typing $ asks (M.lookup name . conMap)
  case lookup of
    Nothing  -> raiseError $ UndefinedCon name
    Just con -> return con

askUserDecl :: VarName -> Typing (Maybe (Located PolyTy))
askUserDecl varname = do
  c <- Typing $ asks ctx
  return $ getUserDecl c varname

askPolyVar :: VarName -> Typing (Maybe (MonoEnv, Ty))
askPolyVar varname = do
  c <- Typing $ asks ctx
  return $ getPolyVar c varname

withUserDecls :: [(VarName, Located PolyTy)] -> Typing a -> Typing a
withUserDecls binds = Typing . local add . unTyping
  where
    add r@(R { ctx }) = r { ctx = addUserDecls ctx binds }

withMonoVars :: Set VarName -> Typing a -> Typing a
withMonoVars vars = Typing . local add . unTyping
  where
    add r@(R { ctx }) = r { ctx = addMonoVars ctx vars }

-- withPolyVars :: [(VarName, (MonoEnv, PolyTy))] -> Typing a -> Typing a
-- withPolyVars vars = Typing . local add . unTyping
--   where
--     add r@(R { ctx }) = r { ctx = addPolyVars ctx vars }

withCtx :: Context -> Typing a -> Typing a
withCtx ctx = Typing . local change . unTyping
  where
    change r = r { ctx = ctx }

withCons :: [(VarName, Ty)] -> Typing a -> Typing a
withCons cons = Typing . local add . unTyping
  where
    add r@(R { conMap }) = r { conMap = conMap `M.union` M.fromList cons }

-- FIXME(taktoa): wow refactor
withClasses cis = withClassMap (M.fromList cis) . withUserDecls vars
  where
    vars = concatMap toVars cis
      where
        toVars (cls, ci) = map (fmap $ addClass (cls, α)) members
          where
            α = clsParam ci
            members = M.toList $ clsMeths ci

            addClass (cls, α) (L loc (PolyTy ctx τ)) = L loc (PolyTy ctx' τ)
              where
                ctx' = (cls, α):ctx

    withClassMap classMap = Typing
                          . local (\ctx -> ctx { classMap })
                          . unTyping

withInstances is = Typing . local add . unTyping
  where
    add r@(R { instances }) = r { instances = instances `M.union` M.fromList is }

askInstance :: Cls -> TyCon -> Typing (Maybe PolyTy)
askInstance cls κ = Typing $ asks (M.lookup (cls, κ) . instances)

askClass :: Cls -> Typing ClsInfo
askClass cls = do
  ci <- Typing $ asks (M.lookup cls . classMap)
  maybe (raiseError $ UndefinedCls cls) return ci

askSupers :: Cls -> Typing [Cls]
askSupers cls = do
  ci <- askClass cls
  clsSupers <$> askClass cls
