module Test.Main
( main
, spec
) where

import Test.Spec.Runner (run)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Thorium.Parse as Thorium.Parse
import Test.Thorium.StatementInterpretation as Thorium.StatementInterpretation
import Thorium.Prelude

main = run [consoleReporter] spec

spec = do
    Thorium.Parse.spec
    Thorium.StatementInterpretation.spec
