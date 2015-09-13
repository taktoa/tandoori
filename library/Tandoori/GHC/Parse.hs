module Tandoori.GHC.Parse (parseMod, getDecls) where

import           Tandoori.GHC.Internals

--import           DynFlags               (Settings (..))
import           Control.Monad.IO.Class (liftIO)
import           GHC
import           GHC.Paths              (libdir)
import           HsSyn                  (hsmodDecls)
import           HsSyn                  (HsModule (..))
import           Lexer                  (ParseResult (..), mkPState, unP)
import           Parser                 (parseModule)
import           RdrName                (RdrName (..))
import           StringBuffer           (hGetStringBuffer)

getDecls mod = hsmodDecls $ unLoc mod

parseMod :: FilePath -> IO (Located (HsModule RdrName))
parseMod srcFile = do
  buf <- hGetStringBuffer srcFile
  let loc = mkRealSrcLoc (mkFastString srcFile) 1 0
  dflags <- runGhc (Just libdir) $ getSessionDynFlags
  case unP Parser.parseModule (mkPState dflags buf loc) of
    POk pst rdrModule       -> return rdrModule
    PFailed srcspan message -> error $ showSDoc dflags message
