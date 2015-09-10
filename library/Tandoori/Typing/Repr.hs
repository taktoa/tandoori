module Tandoori.Typing.Repr (fromHsType) where

import           Tandoori.GHC.Internals    as GHC hiding (isTyVar)
import           Tandoori.Typing
import           Tandoori.Typing.Error
import           Tandoori.Typing.Monad

import qualified TyCon                     as GHC
import qualified TypeRep                   as GHC

import           Control.Applicative
import           Control.Arrow
import           Control.Monad.Trans.Class (MonadTrans)
import           Control.Monad.Writer      (WriterT, lift, runWriterT, tell)


fromHsType :: GHC.HsType GHC.Name -> Typing PolyTy
fromHsType ty = do (τ, ctx) <- runWriterT $ fromHsType' ty
                   return $ PolyTy ctx τ

fromLHsType' :: GHC.LHsType GHC.Name -> WriterT PolyCtx Typing Ty
fromLHsType' = fromHsType' . unLoc

fromHsType' :: GHC.HsType GHC.Name -> WriterT PolyCtx Typing Ty
fromHsType' τ@(GHC.HsTyVar name) | isTyCon τ = return $
                                                 case GHC.wiredInNameTyThing_maybe name of
                                                   Just (GHC.ATyCon tycon) -> if GHC.isTupleTyCon tycon
                                                                              then TyTuple (GHC.tyConArity tycon)
                                                                              else TyCon name
                                                   Nothing -> TyCon name
                                 | isTyVar τ = return $ TyVar name
fromHsType' (GHC.HsFunTy lty1 lty2)          = TyFun <$> (fromHsType' $ unLoc lty1)
                                                     <*> (fromHsType' $ unLoc lty2)
fromHsType' (GHC.HsAppTy lty1 lty2)          = do τ1 <- fromHsType' $ unLoc lty1
                                                  τ2 <- fromHsType' $ unLoc lty2
                                                  return $ TyApp τ1 τ2
fromHsType' (GHC.HsListTy lty)               = tyList <$> fromHsType' (unLoc lty)
fromHsType' (GHC.HsTupleTy _ ltys)           = tyTuple <$> mapM (fromHsType' . unLoc) ltys
fromHsType' (GHC.HsParTy lty)                = fromHsType' $ unLoc lty
fromHsType' (GHC.HsDocTy lty _)              = fromHsType' $ unLoc lty
fromHsType' (GHC.HsOpTy lty1 (_, (L _ op)) lty2) = procOpTy <$> (fromLHsType' lty1)
                                                            <*> (fromHsType' $ GHC.HsTyVar $ op)
                                                            <*> (fromLHsType' lty2)
  where
    procOpTy τ1 τ' τ2 = TyApp (TyApp τ' τ1) τ2
fromHsType' (GHC.HsForAllTy _ _ _ lctx lty)  = mapM (toPolyPred) (unLoc lctx) >>= tell >> (fromHsType' (unLoc lty))
  where
    toPolyPred :: LHsType Name -> WriterT PolyCtx Typing (Name, Name)
    toPolyPred pred = case splitLHsClassTy_maybe pred of
      Just p  -> uncurry classToPolyPred $ first unLoc $ p
      Nothing -> oerr predUnsupportedErr

    classToPolyPred c [L _ τ@(GHC.HsTyVar tv)] | isTyVar τ = return (c, tv)
                                               | otherwise = oerr predNonTVParamErr
    classToPolyPred _ _                                    = oerr predArityErr

    oerr :: String -> WriterT PolyCtx Typing a
    oerr = lift . raiseError . OtherError

predNonTVParamErr, predArityErr, predUnsupportedErr :: String
predNonTVParamErr  = "Predicates should only have type variables for parameters"
predArityErr       = "Predicate with more than one type parameter"
predUnsupportedErr = "Unsupported predicate"



