module Tandoori.Typing.InstanceDecl where

import           Tandoori.GHC.Internals
import           Tandoori.Typing
import           Tandoori.Typing.Error
import           Tandoori.Typing.Monad
import           Tandoori.Typing.Repr

instDecl :: InstDecl Name -> Typing ((Cls, TyCon), PolyTy)
instDecl (InstDecl lty binds lsigs _) = withLSrc lty $ do
  (cls, σ) <- case unLoc lty of
               HsForAllTy e bndr ctx (L _ (HsPredTy (HsClassP cls [lty]))) -> do σ <- fromHsType (HsForAllTy e bndr ctx lty)
                                                                                 return (cls, σ)
               HsPredTy (HsClassP cls [lty])                               -> do σ <- fromHsType $ unLoc lty
                                                                                 return (cls, σ)
               _                                                           -> raiseError InvalidInstance
  let PolyTy _ τ = σ
  case tyCon τ of
    Nothing -> raiseError InvalidInstance
    Just κ -> return ((cls, κ), σ)
