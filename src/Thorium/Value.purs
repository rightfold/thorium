module Thorium.Value
( Value(..)
) where

import Data.Generic (class Generic, gShow)
import Thorium.Prelude

data Value
    = ShouldNotOccur
    | Boolean Boolean

derive instance genericValue :: Generic Value
instance showValue :: Show Value where show = gShow
derive instance eqValue :: Eq Value
derive instance ordValue :: Ord Value
