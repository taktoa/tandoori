module Tandoori.GHC.Parse (parseMod, getDecls) where

import           Tandoori.GHC.Internals

import           DynFlags               (Settings (..), defaultDynFlags)
import           HsSyn                  (hsmodDecls)
import           Lexer                  (ParseResult (..), mkPState, unP)
import           Parser                 (parseModule)
import           StringBuffer           (hGetStringBuffer)

getDecls mod = hsmodDecls $ unLoc mod

parseMod srcFile = do
  buf <- hGetStringBuffer srcFile
  let loc = mkRealSrcLoc (mkFastString srcFile) 1 0
  let dflags = defaultDynFlags (Settings {})
  case unP Parser.parseModule (mkPState dflags buf loc) of
    POk pst rdrModule       -> return rdrModule
    PFailed srcspan message -> error $ showSDoc dflags message
