module Test.Thorium.StatementInterpretation
( spec
) where

import Data.Generic (gEq, gShow)
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
            error <- liftEff $ interpretStatement (CreateInputStream "foo" Text) environment
            error `gShouldEqual` Nothing
            error <- liftEff $ interpretStatement (CreateInputStream "foo" UUID) environment
            error `gShouldEqual` Just (InputStreamAlreadyExists "foo" Text)
            error <- liftEff $ interpretStatement (CreateInputStream "foo" UUID) environment
            error `gShouldEqual` Just (InputStreamAlreadyExists "foo" Text)
        it "CreateOutputStream" do
            environment <- liftEff $ Environment <$> STStrMap.new <*> STStrMap.new
            error <- liftEff $ interpretStatement (CreateOutputStream "foo" Text) environment
            error `gShouldEqual` Nothing
            error <- liftEff $ interpretStatement (CreateOutputStream "foo" UUID) environment
            error `gShouldEqual` Just (OutputStreamAlreadyExists "foo" Text)
            error <- liftEff $ interpretStatement (CreateOutputStream "foo" UUID) environment
            error `gShouldEqual` Just (OutputStreamAlreadyExists "foo" Text)
        it "CreateInputStream and CreateOutputStream" do
            environment <- liftEff $ Environment <$> STStrMap.new <*> STStrMap.new
            error <- liftEff $ interpretStatement (CreateInputStream "foo" Text) environment
            error `gShouldEqual` Nothing
            error <- liftEff $ interpretStatement (CreateOutputStream "foo" Text) environment
            error `gShouldEqual` Nothing

gShouldEqual v1 v2 =
    when (not (v1 `gEq` v2)) $
        fail $ show v1 <> " ≠ " <> show v2
