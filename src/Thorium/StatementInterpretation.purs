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
    = PipeAlreadyExists String Type
    | ReactorAlreadyExists String
    | TypeErrorInReactor TypeError

derive instance genericStatementInterpretationError :: Generic StatementInterpretationError
instance showStatementInterpretationError :: Show StatementInterpretationError where show = gShow

interpretStatement :: ∀ region eff. Statement -> Environment region eff -> Eff (st :: ST region | eff) (StatementInterpretationError + Unit)
interpretStatement (CreatePipe name type_) (Environment pipes _) =
    STStrMap.peek pipes name >>= case _ of
        Nothing -> STStrMap.poke pipes name type_ $> Right unit
        Just existing -> pure $ Left $ PipeAlreadyExists name existing
interpretStatement (CreateReactor name implementation) environment@(Environment _ reactors) =
    STStrMap.peek reactors name >>= case _ of
        Nothing -> do
            runTypeCheck (typeCheckReactor implementation) environment >>= case _ of
                Right _ -> do
                    compileReactor implementation >>= STStrMap.poke reactors name
                    pure $ Right unit
                Left typeError -> pure $ Left $ TypeErrorInReactor typeError
        Just _ -> pure $ Left $ ReactorAlreadyExists name
