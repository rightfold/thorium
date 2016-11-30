module Main
( main
) where

import Control.Monad.Eff.Console (CONSOLE, log, logShow)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.ST (runST)
import Data.StrMap.ST as STStrMap
import Node.Encoding (Encoding(UTF8))
import Node.FS (FS)
import Node.FS.Sync (readTextFile)
import Node.Process (argv, PROCESS)
import Thorium.Environment (Environment(..), newEnvironment)
import Thorium.Parse (parseStatements)
import Thorium.Prelude
import Thorium.Reactor (runReactor)
import Thorium.StatementInterpretation (interpretStatement)
import Thorium.Value (Value(..))

main :: ∀ eff. Eff (console :: CONSOLE, err :: EXCEPTION, fs :: FS, process :: PROCESS | eff) Unit
main = argv >>= case _ of
    [_, bootFilePath] -> boot bootFilePath
    _ -> log "no boot file path given"

boot :: ∀ eff. String -> Eff (console :: CONSOLE, err :: EXCEPTION, fs :: FS | eff) Unit
boot bootFilePath =
    readTextFile UTF8 bootFilePath <#> parseStatements >>= case _ of
        Just statements -> runST do
            environment@(Environment _ _ reactors) <- newEnvironment
            traverse_ (interpretStatement `flip` environment) statements
            STStrMap.peek reactors "load_warnings" >>= case _ of
                Just reactor -> do
                    logShow =<< runReactor reactor ("a" /\ Boolean true)
                    logShow =<< runReactor reactor ("a" /\ Boolean false)
                    logShow =<< runReactor reactor ("in" /\ Boolean true)
                    logShow =<< runReactor reactor ("in" /\ Boolean false)
                    logShow =<< runReactor reactor ("in" /\ Boolean true)
                    logShow =<< runReactor reactor ("in" /\ Boolean false)
                    logShow =<< runReactor reactor ("out" /\ Boolean true)
                    logShow =<< runReactor reactor ("out" /\ Boolean false)
                Nothing -> pure unit
        Nothing -> log "parse error in boot file"
