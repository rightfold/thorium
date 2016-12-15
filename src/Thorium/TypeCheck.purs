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
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.StrMap.ST as STStrMap
import Thorium.Environment (Environment(..))
import Thorium.Prelude
import Thorium.Syntax (Clause(..), Expression(..), From(..), Type(..))

data TypeError
    = UnknownSource String
    | UnknownSink String
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

typeCheckReactor :: ∀ region eff. List From -> List Clause -> TypeCheck region eff Unit
typeCheckReactor froms clauses = typeCheckFroms froms $ typeCheckClauses clauses

typeCheckFroms :: ∀ region eff. List From -> TypeCheck region eff Unit -> TypeCheck region eff Unit
typeCheckFroms Nil next = next
typeCheckFroms (From source name : subsequentFroms) next = do
    Environment pipes _ <- Reader.asks _.environment
    liftEff (STStrMap.peek pipes source) >>= case _ of
        Nothing -> throwError $ UnknownSource source
        Just type_ ->
            Reader.local (\s -> s {variables = Map.insert name type_ s.variables}) $
                typeCheckFroms subsequentFroms next

typeCheckClauses :: ∀ region eff. List Clause -> TypeCheck region eff Unit
typeCheckClauses Nil = pure unit
typeCheckClauses (Distinct part _ : subsequentClauses) =
    typeCheckExpression part *> typeCheckClauses subsequentClauses
typeCheckClauses (Where condition : subsequentClauses) =
    typeCheckExpression condition >>= case _ of
        Boolean -> typeCheckClauses subsequentClauses
        type_ -> throwError $ IncompatibleType type_ Boolean
typeCheckClauses (Select value sink : subsequentClauses) = do
    valueType <- typeCheckExpression value
    Environment pipes _ <- Reader.asks _.environment
    liftEff (STStrMap.peek pipes sink) >>= case _ of
        Nothing -> throwError $ UnknownSink sink
        Just sinkType
            | valueType == sinkType -> typeCheckClauses subsequentClauses
            | otherwise -> throwError $ IncompatibleType valueType sinkType
typeCheckClauses (Scan initial subsequent sink : subsequentClauses) = do
    accumulatorType <- typeCheckExpression initial
    valueType <- Reader.local (_ {accumulator = Just accumulatorType}) $
        typeCheckExpression subsequent
    Environment pipes _ <- Reader.asks _.environment
    liftEff (STStrMap.peek pipes sink) >>= case _ of
        Nothing -> throwError $ UnknownSink sink
        Just sinkType
            | valueType == sinkType -> typeCheckClauses subsequentClauses
            | otherwise -> throwError $ IncompatibleType valueType sinkType

typeCheckExpression :: ∀ region eff. Expression -> TypeCheck region eff Type
typeCheckExpression (Variable name) =
    Reader.asks (_.variables >>> Map.lookup name) >>= case _ of
        Nothing -> throwError $ UnknownVariable name
        Just type_ -> pure type_
typeCheckExpression Accumulator =
    Reader.asks _.accumulator >>= case _ of
        Nothing -> throwError $ AccumulatorOutsideScan
        Just type_ -> pure type_
