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
        it "CreateInputStream" do
            environment <- liftEff newEnvironment
            error <- liftEff $ interpretStatement (CreateInputStream "foo" SinglePrecision) environment
            error `gShouldEqual` Right unit
            error <- liftEff $ interpretStatement (CreateInputStream "foo" DoublePrecision) environment
            error `gShouldEqual` Left (InputStreamAlreadyExists "foo" SinglePrecision)
            error <- liftEff $ interpretStatement (CreateInputStream "foo" DoublePrecision) environment
            error `gShouldEqual` Left (InputStreamAlreadyExists "foo" SinglePrecision)
        it "CreateOutputStream" do
            environment <- liftEff newEnvironment
            error <- liftEff $ interpretStatement (CreateOutputStream "foo" SinglePrecision) environment
            error `gShouldEqual` Right unit
            error <- liftEff $ interpretStatement (CreateOutputStream "foo" DoublePrecision) environment
            error `gShouldEqual` Left (OutputStreamAlreadyExists "foo" SinglePrecision)
            error <- liftEff $ interpretStatement (CreateOutputStream "foo" DoublePrecision) environment
            error `gShouldEqual` Left (OutputStreamAlreadyExists "foo" SinglePrecision)
        it "CreateInputStream and CreateOutputStream" do
            environment <- liftEff newEnvironment
            error <- liftEff $ interpretStatement (CreateInputStream "foo" SinglePrecision) environment
            error `gShouldEqual` Right unit
            error <- liftEff $ interpretStatement (CreateOutputStream "foo" SinglePrecision) environment
            error `gShouldEqual` Right unit

gShouldEqual v1 v2 =
    when (not (v1 `gEq` v2)) $
        fail $ show v1 <> " ≠ " <> show v2
