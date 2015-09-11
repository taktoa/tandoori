{-# LANGUAGE TupleSections #-}

module Tandoori.Typing.ClassDecl (classMap) where

import           Tandoori.GHC.Internals
import           Tandoori.Typing
import           Tandoori.Typing.Error
import           Tandoori.Typing.Monad
import           Tandoori.Typing.Repr

import           Control.Applicative
import           Control.Monad.Error

import           Control.Arrow          (first)
import           Data.Tuple.Extra       (fst3)

import qualified Data.Graph             as G
import qualified Data.Map               as M
import qualified Data.Tree              as T

-- FIXME(taktoa): label and fix all partial expressions

-- TODO: move to separate module
classMap :: [TyClDecl Name] -> Typing [(Cls, ClsInfo)]
classMap decls = do
  (g, fromVertex) <- classGraph decls
  let fromVertex' = tcdName . fromVertex
  let components = G.scc g
  let checkComponent tree = case map fromVertex' (T.flatten tree) of
        [c] -> return c
        cs  -> raiseError $ ClassCycle cs
  mapM_ checkComponent components
  let toClassInfo v = do
        let decl = fromVertex v
        let cls = tcdName decl
        let [L _ (UserTyVar α)] = hsq_tvs $ tcdTyVars decl
        let lsigs = tcdSigs decl
        let supers = map fromVertex' $ G.reachable g v
        meths <- mapM methDecl lsigs
        return (cls, ClsInfo supers α (M.fromList meths))
  -- TODO: Check uniqueness of member names
  mapM toClassInfo $ G.vertices g
  where
    methDecl (L l (TypeSig [L _ nm] (L _ ty) _)) = (nm,) . L l <$> fromHsType ty
    -- FIXME(taktoa): QUESTIONABLE

classGraph :: [TyClDecl Name] -> Typing (G.Graph, G.Vertex -> TyClDecl Name)
classGraph decls = do
  (g, fromVertex, toVertex) <- G.graphFromEdges <$> edges
  let clsFromVertex = fst3 . fromVertex
  return (g, clsFromVertex)
  where
    decls' = filter isClassDecl decls
    edges = mapM edgesFromDecl decls'
    edgesFromDecl decl = do
      checkCtx
      return (decl, cls, map fst ctx)
      where
        cls = tcdName decl
        [L _ (UserTyVar α)] = hsq_tvs $ tcdTyVars decl
        ctx = map (superFromPred) $ unLoc $ tcdCtxt decl
        superFromPred val = case splitLHsClassTy_maybe val of
          Just p  -> uncurry superFromPred' $ first unLoc p
          Nothing -> error "uh, this isn't really handled"
        superFromPred' cls [L _ (HsTyVar α')] = (cls, α')
        checkCtx = forM_ ctx $ \(cls', α') ->
          unless (α' == α) $ raiseError $ InvalidClassCtx (cls, α) (cls', α')


