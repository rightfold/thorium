module Thorium.Reactor
( Reactor
, ReactorOutput(..)
, runReactor
, compileReactor
) where

import Control.Monad.Reader.Class as Reader
import Control.Monad.Reader.Trans (ReaderT, runReaderT)
import Control.Monad.ST (ST, modifySTRef, newSTRef, readSTRef, writeSTRef)
import Control.Monad.Writer.Class as Writer
import Control.Monad.Writer.Trans (runWriterT, WriterT)
import Data.Bifunctor (rmap)
import Data.Generic (class Generic, gShow)
import Data.Map (Map)
import Data.Map as Map
import Data.Set as Set
import Thorium.Prelude
import Thorium.Syntax (Clause(..), Expression(..))
import Thorium.Value (Value(..))

type Reactor region eff =
    ReaderT { message     :: String × Value
            , variables   :: Map String Value
            , accumulator :: Maybe Value
            }
            (WriterT (List ReactorOutput) (Eff (st :: ST region | eff)))

data ReactorOutput
    = IntoInputStream String Value
    | IntoOutputStream String Value

derive instance genericReactorOutput :: Generic ReactorOutput
instance showReactorOutput :: Show ReactorOutput where show = gShow

runReactor
    :: ∀ region eff a
     . Reactor region eff a
    -> String × Value
    -> Eff (st :: ST region | eff) (a × List ReactorOutput)
runReactor action message =
    runWriterT $ runReaderT action {message, variables: Map.empty, accumulator: Nothing}

compileReactor
    :: ∀ region eff reff
     . List Clause
    -> Eff (st :: ST region | eff) (Reactor region reff Unit)
compileReactor Nil = pure (pure unit)
compileReactor (From fromInputStream asName : subsequentClauses) =
    compileReactor subsequentClauses <#> \subsequentReactor -> do
        (actualInputStream /\ value) <- Reader.asks _.message
        when (fromInputStream == actualInputStream) $
            Reader.local (\s -> s {variables = Map.insert asName value s.variables}) subsequentReactor
compileReactor (Distinct onPart _ : subsequentClauses) = do
    recentValues <- newSTRef Set.empty
    compileReactor subsequentClauses <#> \subsequentReactor -> do
        part <- evaluate onPart
        distinct <- not <<< Set.member part <$> liftEff (readSTRef recentValues)
        when distinct $
            liftEff (modifySTRef recentValues (Set.insert part))
            *> subsequentReactor
compileReactor (Where condition : subsequentClauses) =
    compileReactor subsequentClauses <#> \subsequentReactor ->
        evaluate condition
        <#> (_ == Boolean true)
        >>= when `flip` subsequentReactor
compileReactor (SelectIntoInputStream expression inputStream : subsequentClauses) =
    compileReactor subsequentClauses <#> \subsequentReactor -> do
        value <- evaluate expression
        Writer.tell (IntoInputStream inputStream value : Nil)
        subsequentReactor
compileReactor (SelectIntoOutputStream expression outputStream : subsequentClauses) =
    compileReactor subsequentClauses <#> \subsequentReactor -> do
        value <- evaluate expression
        Writer.tell (IntoOutputStream outputStream value : Nil)
        subsequentReactor
compileReactor (ScanIntoInputStream initial subsequent inputStream : subsequentClauses) = do
    ref <- newSTRef Nothing
    compileReactor subsequentClauses <#> \subsequentReactor -> do
        value <- liftEff (readSTRef ref) >>= case _ of
            Nothing -> evaluate initial
            Just accumulator ->
                Reader.local (_ {accumulator = Just accumulator}) $
                    evaluate subsequent
        liftEff $ writeSTRef ref (Just value)
        Writer.tell (IntoInputStream inputStream value : Nil)
compileReactor (ScanIntoOutputStream initial subsequent outputStream : subsequentClauses) = do
    ref <- newSTRef Nothing
    compileReactor subsequentClauses <#> \subsequentReactor -> do
        value <- liftEff (readSTRef ref) >>= case _ of
            Nothing -> evaluate initial
            Just accumulator ->
                Reader.local (_ {accumulator = Just accumulator}) $
                    evaluate subsequent
        liftEff $ writeSTRef ref (Just value)
        Writer.tell (IntoOutputStream outputStream value : Nil)

evaluate :: ∀ region eff. Expression -> Reactor region eff Value
evaluate (Variable name) =
    Reader.asks (fromMaybe ShouldNotOccur <<< Map.lookup name <<< _.variables)
evaluate Accumulator =
    fromMaybe ShouldNotOccur <$> Reader.asks _.accumulator
