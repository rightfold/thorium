module Thorium.StatementInterpretation
( StatementInterpretationError(..)
, interpretStatement
) where

import Control.Monad.ST (ST)
import Data.Generic (class Generic, gShow)
import Data.StrMap.ST as STStrMap
import Thorium.Environment (Environment(..))
import Thorium.Prelude
import Thorium.Syntax (Statement(..), Type)

data StatementInterpretationError
    = InputStreamAlreadyExists String Type
    | OutputStreamAlreadyExists String Type
    | ReactorAlreadyExists String

derive instance genericStatementInterpretationError :: Generic StatementInterpretationError
instance showStatementInterpretationError :: Show StatementInterpretationError where show = gShow

interpretStatement :: ∀ region eff. Statement -> Environment region -> Eff (st :: ST region | eff) (Maybe StatementInterpretationError)
interpretStatement (CreateInputStream name type_) (Environment inputStreams _) =
    STStrMap.peek inputStreams name >>= case _ of
        Nothing -> STStrMap.poke inputStreams name type_ $> Nothing
        Just existing -> pure $ Just $ InputStreamAlreadyExists name existing
interpretStatement (CreateOutputStream name type_) (Environment _ outputStreams) =
    STStrMap.peek outputStreams name >>= case _ of
        Nothing -> STStrMap.poke outputStreams name type_ $> Nothing
        Just existing -> pure $ Just $ OutputStreamAlreadyExists name existing
interpretStatement (CreateReactor name implementation) (Environment _ _) =
    -- Not yet implemented.
    pure Nothing
