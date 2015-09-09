module Tandoori.GHC.Parse (parseMod, getDecls) where

import           Tandoori.GHC.Internals

import           DynFlags               (defaultDynFlags)
import           HsSyn                  (hsmodDecls)
import           Lexer                  (ParseResult (..), mkPState, unP)
import           Parser                 (parseModule)
import           StringBuffer           (hGetStringBuffer)

getDecls mod = hsmodDecls $ unLoc mod

parseMod src_filename = do buf <- hGetStringBuffer src_filename
                           let loc = mkSrcLoc (mkFastString src_filename) 1 0
                               dflags = defaultDynFlags
                           case unP Parser.parseModule (mkPState dflags buf loc) of
                             POk pst rdr_module -> return rdr_module
                             PFailed srcspan message -> error $ showSDoc message
