module Test.Thorium.StatementInterpretation
( spec
) where

import Data.Generic (gEq)
import Data.StrMap.ST as STStrMap
import Test.Spec (describe, it)
import Test.Spec.Assertions (fail)
import Thorium.Environment (Environment(..))
import Thorium.Prelude
import Thorium.StatementInterpretation
import Thorium.Syntax (Statement(..), Type(..))

spec = do
    describe "interpretStatement" do
        it "CreateInputStream" do
            environment <- liftEff $ Environment <$> STStrMap.new <*> STStrMap.new
            error <- liftEff $ interpretStatement (CreateInputStream "foo" SinglePrecision) environment
            error `gShouldEqual` Nothing
            error <- liftEff $ interpretStatement (CreateInputStream "foo" DoublePrecision) environment
            error `gShouldEqual` Just (InputStreamAlreadyExists "foo" SinglePrecision)
            error <- liftEff $ interpretStatement (CreateInputStream "foo" DoublePrecision) environment
            error `gShouldEqual` Just (InputStreamAlreadyExists "foo" SinglePrecision)
        it "CreateOutputStream" do
            environment <- liftEff $ Environment <$> STStrMap.new <*> STStrMap.new
            error <- liftEff $ interpretStatement (CreateOutputStream "foo" SinglePrecision) environment
            error `gShouldEqual` Nothing
            error <- liftEff $ interpretStatement (CreateOutputStream "foo" DoublePrecision) environment
            error `gShouldEqual` Just (OutputStreamAlreadyExists "foo" SinglePrecision)
            error <- liftEff $ interpretStatement (CreateOutputStream "foo" DoublePrecision) environment
            error `gShouldEqual` Just (OutputStreamAlreadyExists "foo" SinglePrecision)
        it "CreateInputStream and CreateOutputStream" do
            environment <- liftEff $ Environment <$> STStrMap.new <*> STStrMap.new
            error <- liftEff $ interpretStatement (CreateInputStream "foo" SinglePrecision) environment
            error `gShouldEqual` Nothing
            error <- liftEff $ interpretStatement (CreateOutputStream "foo" SinglePrecision) environment
            error `gShouldEqual` Nothing

gShouldEqual v1 v2 =
    when (not (v1 `gEq` v2)) $
        fail $ show v1 <> " ≠ " <> show v2
