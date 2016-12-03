module Test.Thorium.StatementInterpretation
( spec
) where

import Data.Generic (gEq)
import Data.StrMap.ST as STStrMap
import Test.Spec (describe, it)
import Test.Spec.Assertions (fail)
import Thorium.Environment (newEnvironment)
import Thorium.Prelude
import Thorium.StatementInterpretation
import Thorium.Syntax (Statement(..), Type(..))

spec = do
    describe "interpretStatement" do
        it "CreateStream" do
            environment <- liftEff newEnvironment
            error <- liftEff $ interpretStatement (CreateStream "foo" SinglePrecision) environment
            error `gShouldEqual` Right unit
            error <- liftEff $ interpretStatement (CreateStream "foo" DoublePrecision) environment
            error `gShouldEqual` Left (StreamAlreadyExists "foo" SinglePrecision)
            error <- liftEff $ interpretStatement (CreateStream "foo" DoublePrecision) environment
            error `gShouldEqual` Left (StreamAlreadyExists "foo" SinglePrecision)

gShouldEqual v1 v2 =
    when (not (v1 `gEq` v2)) $
        fail $ show v1 <> " ≠ " <> show v2
