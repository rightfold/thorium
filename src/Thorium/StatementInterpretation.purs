module Thorium.StatementInterpretation
( StatementInterpretationError(..)
, interpretStatement
) where

import Control.Monad.ST (ST)
import Data.Generic (class Generic, gShow)
import Data.StrMap.ST as STStrMap
import Thorium.Environment (Environment(..))
import Thorium.Prelude
import Thorium.Reactor (compileReactor)
import Thorium.Syntax (Statement(..), Type)
import Thorium.TypeCheck (runTypeCheck, typeCheckReactor, TypeError)

data StatementInterpretationError
    = InputStreamAlreadyExists String Type
    | OutputStreamAlreadyExists String Type
    | ReactorAlreadyExists String
    | TypeErrorInReactor TypeError

derive instance genericStatementInterpretationError :: Generic StatementInterpretationError
instance showStatementInterpretationError :: Show StatementInterpretationError where show = gShow

interpretStatement :: ∀ region eff. Statement -> Environment region eff -> Eff (st :: ST region | eff) (Maybe StatementInterpretationError)
interpretStatement (CreateInputStream name type_) (Environment inputStreams _ _) =
    STStrMap.peek inputStreams name >>= case _ of
        Nothing -> STStrMap.poke inputStreams name type_ $> Nothing
        Just existing -> pure $ Just $ InputStreamAlreadyExists name existing
interpretStatement (CreateOutputStream name type_) (Environment _ outputStreams _) =
    STStrMap.peek outputStreams name >>= case _ of
        Nothing -> STStrMap.poke outputStreams name type_ $> Nothing
        Just existing -> pure $ Just $ OutputStreamAlreadyExists name existing
interpretStatement (CreateReactor name implementation) environment@(Environment _ _ reactors) =
    STStrMap.peek reactors name >>= case _ of
        Nothing -> do
            runTypeCheck (typeCheckReactor implementation) environment >>= case _ of
                Right _ -> do
                    compileReactor implementation >>= STStrMap.poke reactors name
                    pure Nothing
                Left typeError -> pure $ Just $ TypeErrorInReactor typeError
        Just _ -> pure $ Just $ ReactorAlreadyExists name
