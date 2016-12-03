module Thorium.Prelude
( module Control.Monad.Eff
, module Control.Monad.Eff.Class
, module Data.Either
, module Data.Foldable
, module Data.List
, module Data.Maybe
, module Data.Tuple
, module Data.Tuple.Nested
, module Data.Traversable
, module Prelude
, type (×)
, type (+)
) where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Data.Either (Either(..))
import Data.Foldable (any)
import Data.List ((:), List(Nil))
import Data.Maybe (fromMaybe, Maybe(..))
import Data.Traversable (traverse)
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
import Prelude

infixr 7 type Tuple as ×
infixr 6 type Either as +
