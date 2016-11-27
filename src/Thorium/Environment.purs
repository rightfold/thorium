module Thorium.Environment
( Environment(..)
) where

import Data.StrMap.ST (STStrMap)
import Thorium.Syntax (Type)

data Environment region = Environment (STStrMap region Type) (STStrMap region Type)
