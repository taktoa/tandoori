{-# LANGUAGE LambdaCase #-}

module Tandoori.Typing.DataType (constructorsFromDecl) where

import           Tandoori.Aliases
import           Tandoori.GHC.Internals
import           Tandoori.Typing
import           Tandoori.Typing.Error
import           Tandoori.Typing.Monad
import           Tandoori.Typing.Repr

import           Control.Monad

constructorsFromDecl :: TyClDecl Name -> Typing [(ConName, Ty)]
constructorsFromDecl decl | isDataDecl decl = do
  let nameData = tcdName decl
  let αs = hsLTyVarNames $ map unLoc $ tcdTyVars decl
  let τd = tyCurryApp $ TyCon nameData : map TyVar αs

  forM (map unLoc $ (dd_cons . tcdDataDefn) decl) $ \con -> do
    let tys = map unLoc $ hsConDeclArgTys $ con_details con
    ςa <- mapM fromHsType tys
    τa <- forM ςa $ \case ς@(PolyTy []  τ) -> return τ
                          ς@(PolyTy ctx τ) -> raiseError $ InvalidCon ς
    let τ = tyCurryFun (τa ++ [τd])
    return (unLoc $ head $ con_names con, τ)
constructorsFromDecl _ = return []

--tcdCons = _ . dd_cons . tcdDataDefn
