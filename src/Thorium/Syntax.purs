module Thorium.Syntax
( Statement(..)
, Expression(..)
, Clause(..)
, Within(..)
, Type(..)
) where

import Data.Generic (class Generic, gShow)
import Thorium.Prelude

data Statement
    = CreateStream String Type
    | CreateReactor String (List Clause)

data Expression
    = Variable String
    | Accumulator

data Clause
    = From String String
    | Distinct Expression Within
    | Where Expression
    | Select Expression String
    | Scan Expression Expression String

data Within = Infinity | Elements Expression | Period Expression

data Type
    = Record (List (String × Type))
    | Boolean
    | SinglePrecision
    | DoublePrecision

derive instance genericStatement  :: Generic Statement
derive instance genericExpression :: Generic Expression
derive instance genericClause     :: Generic Clause
derive instance genericWithin     :: Generic Within
derive instance genericType       :: Generic Type

instance showStatement  :: Show Statement  where show = gShow
instance showExpression :: Show Expression where show = gShow
instance showClause     :: Show Clause     where show = gShow
instance showWithin     :: Show Within     where show = gShow
instance showType       :: Show Type       where show = gShow

derive instance eqType :: Eq Type
