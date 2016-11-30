module Thorium.Environment
( Environment(..)
, newEnvironment
) where

import Control.Monad.ST (ST)
import Data.StrMap.ST (STStrMap)
import Data.StrMap.ST as STStrMap
import Thorium.Prelude
import Thorium.Reactor (Reactor)
import Thorium.Syntax (Type)

data Environment region eff = Environment (STStrMap region Type) (STStrMap region Type) (STStrMap region (Reactor region eff Unit))

newEnvironment :: ∀ region eff reff. Eff (st :: ST region | eff) (Environment region reff)
newEnvironment = Environment <$> STStrMap.new <*> STStrMap.new <*> STStrMap.new
