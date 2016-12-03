module Test.Thorium.Parse
( spec
) where

import Data.Generic (gEq)
import Test.Spec (describe, it)
import Test.Spec.Assertions (fail)
import Thorium.Parse
import Thorium.Prelude
import Thorium.Syntax (Statement(..), Type(..))

spec = do
    describe "parseStatement" do
        it "CreateStream" do
            let result = parseStatement "CREATE STREAM load SINGLE PRECISION;"
            result `gShouldEqual` Right (CreateStream "load" SinglePrecision)

gShouldEqual v1 v2 =
    when (not (v1 `gEq` v2)) $
        fail $ show v1 <> " ≠ " <> show v2
