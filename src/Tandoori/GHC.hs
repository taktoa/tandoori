module Tandoori.GHC (runDyn) where

import           DynFlags  (defaultDynFlags)
import           GHC       (defaultErrorHandler, getSessionDynFlags, runGhc,
                            setSessionDynFlags)
import           GHC.Paths (libdir)

runDyn m = defaultErrorHandler defaultDynFlags $ do
             runGhc (Just libdir) $ do
               --newSession
               dflags <- getSessionDynFlags
               _ <- setSessionDynFlags dflags
               -- env <- liftIO $ newHscEnv dflags
               -- setSession env
               m
