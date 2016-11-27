module Main
( main
) where

import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION)
import Node.Encoding (Encoding(UTF8))
import Node.FS (FS)
import Node.FS.Sync (readTextFile)
import Node.Process (argv, PROCESS)
import Thorium.Parse (parseStatements)
import Thorium.Prelude

main :: ∀ eff. Eff (console :: CONSOLE, err :: EXCEPTION, fs :: FS, process :: PROCESS | eff) Unit
main = argv >>= case _ of
    [_, bootFilePath] -> boot bootFilePath
    _ -> log "no boot file path given"

boot :: ∀ eff. String -> Eff (console :: CONSOLE, err :: EXCEPTION, fs :: FS | eff) Unit
boot bootFilePath = do
    statements <- parseStatements <$> readTextFile UTF8 bootFilePath
    log $ show statements
