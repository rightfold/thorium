module Thorium.TypeCheck
( TypeError(..)
, TypeCheck
, runTypeCheck
, typeCheckReactor
, typeCheckExpression
) where

import Control.Monad.Error.Class (throwError)
import Control.Monad.Except.Trans (ExceptT, runExceptT)
import Control.Monad.Reader.Class as Reader
import Control.Monad.Reader.Trans (ReaderT, runReaderT)
import Control.Monad.ST (ST)
import Data.Generic (class Generic, gShow)
import Data.Map (Map)
import Data.Map as Map
import Data.StrMap.ST as STStrMap
import Thorium.Environment (Environment(..))
import Thorium.Prelude
import Thorium.Syntax (Clause(..), Expression(..), Type(..))

data TypeError
    = UnknownStream String
    | UnknownVariable String
    | IncompatibleType Type Type
    | AccumulatorOutsideScan

derive instance genericTypeError :: Generic TypeError
instance showTypeError :: Show TypeError where show = gShow

type TypeCheck region eff =
    ReaderT { environment :: Environment region eff
            , variables   :: Map String Type
            , accumulator :: Maybe Type
            }
            (ExceptT TypeError (Eff (st :: ST region | eff)))

runTypeCheck
    :: ∀ region eff a
     . TypeCheck region eff a
    -> Environment region eff
    -> Eff (st :: ST region | eff) (TypeError + a)
runTypeCheck action environment =
    runExceptT $ runReaderT action {environment, variables: Map.empty, accumulator: Nothing}

typeCheckReactor :: ∀ region eff. List Clause -> TypeCheck region eff Unit
typeCheckReactor Nil = pure unit
typeCheckReactor (From stream name : subsequentClauses) = do
    Environment streams _ <- Reader.asks _.environment
    liftEff (STStrMap.peek streams stream) >>= case _ of
        Nothing -> throwError $ UnknownStream stream
        Just type_ ->
            Reader.local (\s -> s {variables = Map.insert name type_ s.variables}) $
                typeCheckReactor subsequentClauses
typeCheckReactor (Distinct part _ : subsequentClauses) =
    typeCheckExpression part *> typeCheckReactor subsequentClauses
typeCheckReactor (Where condition : subsequentClauses) =
    typeCheckExpression condition >>= case _ of
        Boolean -> typeCheckReactor subsequentClauses
        type_ -> throwError $ IncompatibleType type_ Boolean
typeCheckReactor (Select value stream : subsequentClauses) = do
    valueType <- typeCheckExpression value
    Environment streams _ <- Reader.asks _.environment
    liftEff (STStrMap.peek streams stream) >>= case _ of
        Nothing -> throwError $ UnknownStream stream
        Just streamType
            | valueType == streamType -> typeCheckReactor subsequentClauses
            | otherwise -> throwError $ IncompatibleType valueType streamType
typeCheckReactor (Scan initial subsequent stream : subsequentClauses) = do
    accumulatorType <- typeCheckExpression initial
    valueType <- Reader.local (_ {accumulator = Just accumulatorType}) $
        typeCheckExpression subsequent
    Environment streams _ <- Reader.asks _.environment
    liftEff (STStrMap.peek streams stream) >>= case _ of
        Nothing -> throwError $ UnknownStream stream
        Just streamType
            | valueType == streamType -> typeCheckReactor subsequentClauses
            | otherwise -> throwError $ IncompatibleType valueType streamType

typeCheckExpression :: ∀ region eff. Expression -> TypeCheck region eff Type
typeCheckExpression (Variable name) =
    Reader.asks (_.variables >>> Map.lookup name) >>= case _ of
        Nothing -> throwError $ UnknownVariable name
        Just type_ -> pure type_
typeCheckExpression Accumulator =
    Reader.asks _.accumulator >>= case _ of
        Nothing -> throwError $ AccumulatorOutsideScan
        Just type_ -> pure type_
