module Thorium.Environment
( Environment(..)
, newEnvironment
) where

import Control.Monad.ST (ST)
import Data.StrMap.ST (STStrMap)
import Data.StrMap.ST as STStrMap
import Thorium.Prelude
import Thorium.Syntax (Type)

data Environment region = Environment (STStrMap region Type) (STStrMap region Type)

newEnvironment :: ∀ region eff. Eff (st :: ST region | eff) (Environment region)
newEnvironment = Environment <$> STStrMap.new <*> STStrMap.new
