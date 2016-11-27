module Test.Main
( main
, spec
) where

import Test.Spec.Runner (run)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Thorium.StatementInterpretation as Thorium.StatementInterpretation

main = run [consoleReporter] spec

spec = do
    Thorium.StatementInterpretation.spec
