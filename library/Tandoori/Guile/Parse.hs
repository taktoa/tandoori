{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE QuasiQuotes      #-}
{-# LANGUAGE TemplateHaskell  #-}

module Tandoori.Guile.Parse where

import           Data.Attoparsec.Text
import           Data.Text            (Text)

data Program

data LGuile a = LGuile (Guile a) SourceSpan

data Guile a where
  GProgram     :: [Guile Form]     -> Guile Program
  GDefinition  :: Guile Definition -> Guile Form
  GExpression  :: Guile Expression -> Guile Form
  GVariableDef :: Guile


data GuileToken = GTStringDelim
                | GTCharacter Char
                | GTOpenParen
                | GTCloseParen

tokenize :: Text -> [GuileToken]

-- <empty>
-- <program>
-- <form>
-- <definition>
-- <expression>
-- <variable definition>
-- <syntax definition>
-- <syntax binding>
-- <derived definition>
-- <variable>
-- <body>
-- <identifier>
-- <keyword>
-- <transformer expression>
-- <constant>
-- <datum>
-- <formals>
-- <application>
-- <derived expression>
-- <boolean>
-- <number>
-- <character>
-- <string>
-- <initial>
-- <subsequent>
-- <letter>
-- < | = | >
-- <digit>
-- <symbol>
-- <list>
-- <vector>
-- <num 2>
-- <num 8>
-- <num 10>
-- <num 16>
-- <any character>
-- <string character>
-- <any character other than " or \>
-- <abbreviation>
-- <decimal r>
-- <num r>
-- <prefix r>
-- <complex r>
-- <real r>
-- <imag r>
-- <ureal r>
-- <sign>
-- <uinteger r>
-- <digit r>
-- <radix r>
-- <exactness>
-- <decimal 10>
-- <uinteger 10>
-- <exponent>
-- <digit 10>
-- <suffix>
-- <exponent marker>
