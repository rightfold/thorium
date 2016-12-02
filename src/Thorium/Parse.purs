module Thorium.Parse
( parseStatements
, parseStatement
) where

import Thorium.Prelude
import Thorium.Syntax (Statement)

foreign import parseStatements :: String -> Either String (Array Statement)
foreign import parseStatement :: String -> Either String Statement
