module Tandoori.GHC (runDyn) where

import           DynFlags  (defaultFatalMessager, defaultFlushOut)
import           GHC       (defaultErrorHandler, getSessionDynFlags, runGhc,
                            setSessionDynFlags)
import           GHC.Paths (libdir)

runDyn m = errHandle $ runGhc (Just libdir) $ do
  --newSession
  dflags <- getSessionDynFlags
  _ <- setSessionDynFlags dflags
  -- env <- liftIO $ newHscEnv dflags
  -- setSession env
  m

errHandle = defaultErrorHandler defaultFatalMessager defaultFlushOut
