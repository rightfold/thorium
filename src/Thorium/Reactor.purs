module Thorium.Reactor
( Reactor
, ReactorOutput(..)
, runReactor
, compileReactor
) where

import Control.Monad.Reader.Class as Reader
import Control.Monad.Reader.Trans (ReaderT, runReaderT)
import Control.Monad.ST (ST, modifySTRef, newSTRef, readSTRef)
import Control.Monad.Writer.Class as Writer
import Control.Monad.Writer.Trans (runWriterT, WriterT)
import Data.Bifunctor (rmap)
import Data.Map (Map)
import Data.Map as Map
import Data.Set as Set
import Thorium.Prelude
import Thorium.Syntax (Clause(..), Expression(..))
import Thorium.Value (Value(..))

type Reactor region eff = ReaderT ((String × Value) × Map String Value) (WriterT (List ReactorOutput) (Eff (st :: ST region | eff)))

data ReactorOutput
    = IntoInputStream String Value
    | IntoOutputStream String Value

runReactor
    :: ∀ region eff a
     . String × Value
    -> Reactor region eff a
    -> Eff (st :: ST region | eff) (a × List ReactorOutput)
runReactor message action =
    runWriterT $ runReaderT action (message /\ Map.empty)

compileReactor
    :: ∀ region eff reff
     . List Clause
    -> Eff (st :: ST region | eff) (Reactor region reff Unit)
compileReactor Nil = pure (pure unit)
compileReactor (From fromInputStream asName : subsequentClauses) =
    compileReactor subsequentClauses <#> \subsequentReactor -> do
        (actualInputStream /\ value) <- Reader.asks fst
        when (fromInputStream == actualInputStream) $
            Reader.local (rmap $ Map.insert asName value) subsequentReactor
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

evaluate :: ∀ region eff. Expression -> Reactor region eff Value
evaluate (Variable name) =
    Reader.asks (fromMaybe (Boolean false) <<< Map.lookup name <<< snd)
