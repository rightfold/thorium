module Thorium.Parse
( parseStatements
, parseStatement
) where

import Thorium.Prelude
import Thorium.Syntax (Statement)

foreign import parseStatements :: String -> Maybe (Array Statement)
foreign import parseStatement :: String -> Maybe Statement
