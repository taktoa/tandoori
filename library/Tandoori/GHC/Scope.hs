module Tandoori.GHC.Scope (runScope) where

import           Bag             (emptyBag)
import           Data.IORef      (newIORef)
import qualified Data.Set        as Set (empty)
import           DriverPhases    (HscSource (..))
import           FamInstEnv      (emptyFamInstEnv)
import           FastString      (fsLit)
import           GHC             (HsDecl (..), HsModule (..), emptyLHsBinds,
                                  emptyRnGroup, mkModule, mkModuleName)
import           HscTypes        (HscEnv, Warnings (..), hsc_type_env_var)
import           HsDecls         (HsGroup (..), LTyClDecl, mkTyClGroup,
                                  tyClGroupConcat)
import           HsImpExp        (LImportDecl)
import           InstEnv         (emptyInstEnv)
import           Module          (mainPackageKey)
import           Name            (Name)
import           NameEnv         (emptyNameEnv)
import           NameSet         (emptyDUs, emptyNameSet)
import           OccName         (emptyOccSet)
import           Panic           (panic)
import           RdrName         (RdrName, emptyGlobalRdrEnv, emptyLocalRdrEnv,
                                  extendLocalRdrEnvList)
import           RnSource        (findSplice, rnSrcDecls, rnTyClDecls)
import           SrcLoc          (GenLocated (..), Located (..),
                                  mkGeneralSrcSpan, unLoc)
import           TcRnMonad       (TcGblEnv (..), TcLclEnv (..), initTcRnIf)
import           TcRnTypes       (ArrowCtxt (..), RecFieldEnv (..),
                                  emptyImportAvails, topStage)
import           VarSet          (emptyVarSet)

import           Tandoori.Typing

builtinNames = builtinTyNames ++ builtinDataConNames ++ builtinClassNames

mkLcl = do errs_var <- newIORef (emptyBag, emptyBag)
           tvs_var  <- newIORef emptyVarSet
           return $ TcLclEnv { tcl_errs       = errs_var
                             -- , tcl_loc        = mkGeneralSrcSpan (fsLit "Top level")
                             , tcl_ctxt       = []
                             , tcl_rdr        = emptyLocalRdrEnv `extendLocalRdrEnvList` builtinNames
                             , tcl_th_ctxt    = topStage
                             , tcl_arrow_ctxt = NoArrowCtxt
                             , tcl_env        = emptyNameEnv
                             , tcl_tyvars     = tvs_var
                             , tcl_lie        = panic "tcl_lie"

                                                -- Add:
                                                -- tcl_tclvl :: TcLevel
                                                -- tcl_th_bndrs :: ThBindEnv
                                                -- tcl_bndrs :: [TcIdBinder]
                                                -- tcl_tidy :: TidyEnv

                                                -- Removed:
                                                -- tcl_meta       = panic "tcl_meta",
                                                -- tcl_untch      = panic "tcl_untch"
                             }

-- | This is terrible and dangerous
-- mkRealGeneralSrcSpan :: FastString -> RealSrcSpan
-- mkReadGeneralSrcSpan



mkGbl env mod = do dfuns_var         <- newIORef emptyNameSet
                   keep_var          <- newIORef emptyNameSet
                   used_rdrnames     <- newIORef Set.empty
                   th_var            <- newIORef False
                   dfun_n_var        <- newIORef emptyOccSet
                   type_env_var      <- case hsc_type_env_var env of
                                         Just (_mod, te_var) -> return te_var
                                         Nothing             -> newIORef emptyNameEnv
                   return $ TcGblEnv {
                                tcg_mod       = mod,
                                tcg_src       = HsSrcFile,
                                tcg_rdr_env   = emptyGlobalRdrEnv,
                                tcg_fix_env   = emptyNameEnv,
                                tcg_field_env = RecFields emptyNameEnv emptyNameSet,
                                tcg_default   = Nothing,
                                tcg_type_env  = panic "tcg_type_env",
                                tcg_type_env_var = type_env_var,
                                tcg_inst_env  = emptyInstEnv,
                                tcg_fam_inst_env  = emptyFamInstEnv,
                                -- tcg_inst_uses = dfuns_var,
                                tcg_th_used   = th_var,
                                tcg_exports  = [],
                                tcg_imports  = emptyImportAvails,
                                tcg_used_rdrnames = used_rdrnames,
                                tcg_dus      = emptyDUs,

                                tcg_rn_imports = [],
                                tcg_rn_exports = Just [],
                                tcg_rn_decls   = Just emptyRnGroup,

                                tcg_binds    = emptyLHsBinds,
                                tcg_warns    = NoWarnings,
                                tcg_anns     = [],
                                tcg_insts    = [],
                                tcg_fam_insts= [],
                                tcg_rules    = [],
                                tcg_fords    = [],
                                tcg_dfun_n   = dfun_n_var,
                                tcg_keep     = keep_var,
                                tcg_doc_hdr  = Nothing,
                                tcg_hpc      = False,

                                tcg_ev_binds = panic "tcg_ev_bind",
                                tcg_sigs = panic "tcg_sigs",
                                tcg_imp_specs = panic "tcg_imp_specs",
                                tcg_main = panic "tcg_main"
                              }

runScope :: HscEnv -> Located (HsModule RdrName) -> IO ([LImportDecl Name], [LTyClDecl Name], HsGroup Name)
runScope env lmod = do let modinfo = mkModule mainPackageKey $ mkModuleName "Main"
                           -- group = fst $ findSplice decls
                       gbl <- mkGbl env modinfo
                       lcl <- mkLcl

                       -- (rn_imports, rdr_env, _, _) <- initTcRnIf 'a' env gbl lcl $ checkNoErrs $ rnImports imports

                       (gbl', group') <- initTcRnIf 'a' env gbl lcl $ do
                         (group, _) <- findSplice decls
                         rnSrcDecls [] group -- FIXME(taktoa): QUESTIONABLE
                       (tydecls', _) <- initTcRnIf 'a' env gbl' lcl $ rnTyClDecls [] $ [mkTyClGroup ltycldecls]
                       return (error "Unsupported: imports", tyClGroupConcat tydecls', group')

    where mod = unLoc lmod
          imports = hsmodImports mod
          decls = hsmodDecls mod
          tycldecls = filter (isTyDecl . unLoc) decls
          ltycldecls = map (\(L loc (TyClD decl)) -> L loc decl) tycldecls
          isTyDecl (TyClD _) = True
          isTyDecl _ = False

