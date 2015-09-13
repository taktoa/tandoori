{-# LANGUAGE TupleSections #-}

module Tandoori.Typing.InstanceDecl where

import           Tandoori.GHC.Internals
import           Tandoori.Typing
import           Tandoori.Typing.Error
import           Tandoori.Typing.Monad
import           Tandoori.Typing.Repr

import           Control.Arrow
import           Data.Maybe

import           HsDecls

instDecl :: InstDecl Name -> Typing ((Cls, TyCon), PolyTy)
instDecl (ClsInstD (ClsInstDecl lty binds lsigs _ _ _)) = withLSrc lty $ do
  (cls, ς) <- case unLoc lty of
    HsForAllTy e ss bndr ctx ty -> lookupPair $ HsForAllTy e ss bndr ctx lty
    _                           -> raiseError InvalidInstance
  let PolyTy _ τ = ς
  case tyCon τ of
    Nothing -> raiseError InvalidInstance
    Just κ  -> return ((cls, κ), ς)
  where
    lookupPair :: HsType Name -> Typing (Name, PolyTy)
    lookupPair = lhsTypesToTys . getHsClassTy

    lhsTypesToTys :: (Name, [LHsType Name]) -> Typing (Name, PolyTy)
    lhsTypesToTys (n, xs) = mapM (fromHsType . unLoc) xs >>= \l -> return (n, head l)

    getHsClassTy :: HsType name -> (name, [LHsType name])
    getHsClassTy = head . maybeToList . splitHsClassTy_maybe
