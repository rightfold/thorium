module Thorium.Reactor
( Reactor
, ReactorOutput
, runReactor
, compileReactor
) where

import Control.Monad.Reader.Class as Reader
import Control.Monad.Reader.Trans (ReaderT, runReaderT)
import Control.Monad.ST (ST, modifySTRef, newSTRef, readSTRef, writeSTRef)
import Control.Monad.Writer.Class as Writer
import Control.Monad.Writer.Trans (runWriterT, WriterT)
import Data.List as List
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

type ReactorOutput = String × Value

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
compileReactor (From fromSource asName : subsequentClauses) =
    compileReactor subsequentClauses <#> \subsequentReactor -> do
        (actualSource /\ value) <- Reader.asks _.message
        when (fromSource == actualSource) $
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
compileReactor (Select expression sink : subsequentClauses) =
    compileReactor subsequentClauses <#> \subsequentReactor -> do
        value <- evaluate expression
        Writer.tell (List.singleton $ sink /\ value)
        subsequentReactor
compileReactor (Scan initial subsequent sink : subsequentClauses) = do
    ref <- newSTRef Nothing
    compileReactor subsequentClauses <#> \subsequentReactor -> do
        value <- liftEff (readSTRef ref) >>= case _ of
            Nothing -> evaluate initial
            Just accumulator ->
                Reader.local (_ {accumulator = Just accumulator}) $
                    evaluate subsequent
        liftEff $ writeSTRef ref (Just value)
        Writer.tell (List.singleton $ sink /\ value)

evaluate :: ∀ region eff. Expression -> Reactor region eff Value
evaluate (Variable name) =
    Reader.asks (fromMaybe ShouldNotOccur <<< Map.lookup name <<< _.variables)
evaluate Accumulator =
    fromMaybe ShouldNotOccur <$> Reader.asks _.accumulator
