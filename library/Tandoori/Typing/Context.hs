{-# LANGUAGE NamedFieldPuns #-}

-- | Type contexts
module Tandoori.Typing.Context ( Context
                               , mkContext
                               , monoVars
                               , addMonoVars
                               , polyVars
                               , getPolyVar
                               , addPolyVars
                               , userDecls
                               , getUserDecl
                               , addUserDecls
                               ) where

import           Tandoori.Aliases
import           Tandoori.GHC.Internals  (Located)
import           Tandoori.Typing
import           Tandoori.Typing.MonoEnv

import           Data.Map                (Map)
import           Data.Set                (Set)

import qualified Data.Map                as M
import qualified Data.Set                as S

data Context = Context { monoVars  :: Set VarName
                       , polyVars  :: Map VarName (MonoEnv, Ty)
                       , userDecls :: Map VarName (Located PolyTy)
                       }

mkContext :: Context
mkContext = Context { monoVars  = S.empty
                    , polyVars  = M.empty
                    , userDecls = M.empty }

addMonoVars ctx@(Context { monoVars }) vars
  = ctx { monoVars = monoVars `S.union` vars }

addPolyVars :: Context -> [(VarName, (MonoEnv, Ty))] -> Context
addPolyVars ctx@(Context { polyVars }) vars
  = ctx { polyVars = polyVars `M.union` M.fromList vars }

getPolyVar :: Context -> VarName -> Maybe (MonoEnv, Ty)
getPolyVar (Context { polyVars }) varname = M.lookup varname polyVars

getUserDecl :: Context -> VarName -> Maybe (Located PolyTy)
getUserDecl (Context { userDecls }) varname = M.lookup varname userDecls

addUserDecls :: Context -> [(VarName, Located PolyTy)] -> Context
addUserDecls ctx@(Context { userDecls }) binds
  = ctx { userDecls = foldl addDecl userDecls binds }

addDecl decls (name, lσ) = M.insert name lσ decls
