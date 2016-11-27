module Thorium.Prelude
( module Control.Monad.Eff
, module Control.Monad.Eff.Class
, module Data.List
, module Data.Maybe
, module Prelude
, type (×)
) where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Data.List (List)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple)
import Prelude

infixr 7 type Tuple as ×
