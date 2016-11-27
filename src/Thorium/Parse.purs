module Thorium.Parse
( parseStatement
) where

import Thorium.Prelude
import Thorium.Syntax (Statement)

foreign import parseStatement :: String -> Maybe Statement
