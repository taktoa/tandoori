-- | Re-exports many modules
module Tandoori.GHC.Internals ( module SrcLoc
                              , module Outputable
                              , module Name
                              , module BasicTypes
                              , module Unique
                              , module FastString
                              , module HsExpr
                              , module HsDecls
                              , module HsTypes
                              , module HsPat
                              , module HsLit
                              , module HsBinds
                              , module DataCon
                              , module TysWiredIn
                              , module PrelNames
                              , module Module
                              ) where

import           BasicTypes
import           DataCon    (dataConName)
import           FastString
import           HsBinds
import           HsDecls
import           HsExpr
import           HsLit
import           HsPat
import           HsTypes
import           Module
import           Name
import           Outputable
import           PrelNames  (eqClassName, fractionalClassName, numClassName,
                             ordClassName, stringTyConName)
import           SrcLoc
import           TysWiredIn (boolTyConName, charTyConName, consDataCon,
                             falseDataCon, intTyConName, listTyConName,
                             nilDataCon, trueDataCon, tupleTyCon)
import           Unique
