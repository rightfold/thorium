module Thorium.Value
( Value(..)
) where

import Thorium.Prelude

data Value
    = Boolean Boolean

derive instance eqValue :: Eq Value
derive instance ordValue :: Ord Value
