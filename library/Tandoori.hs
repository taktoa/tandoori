{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- | Main entry point to Tandoori
module Tandoori (module Tandoori) where

import           Tandoori.GHC
import           Tandoori.GHC.Parse
import           Tandoori.GHC.Scope

import           GHC
import           IOEnv
import           Outputable

import           System.Environment

import           Tandoori.Typing.Infer
import           Tandoori.Typing.Show

typecheckMod mod = runDyn $ do
  env <- getSession
  (limports, ltydecls, group) <- liftIO $ runScope env mod
  return $ infer (map unLoc ltydecls) group

main' [srcFile] = do
  mod <- parseMod srcFile
  (c, errors) <- typecheckMod mod
  if not $ null errors
    then mapM_ (\err -> printErrs $ ppr err $ mkErrStyle neverQualify) errors
    else return ()
  case c of
    Just (ctxt, m) -> printCtxt ctxt
    Nothing        -> return ()
  return c

main' _ = error "Usage: tandoori <haskell-file>"

-- | Main entry point of application
main = do
  args <- getArgs
  main' args
