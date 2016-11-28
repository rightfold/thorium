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
    = CreateInputStream String Type
    | CreateOutputStream String Type
    | CreateReactor String (List Clause)

data Expression
    = Variable String

data Clause
    = From String String
    | Distinct (Maybe Expression) Within
    | Where Expression
    | SelectIntoInputStream (Maybe Expression) String
    | SelectIntoOutputStream (Maybe Expression) String

data Within = Infinity | Elements Expression | Period Expression

data Type
    = Record (List (String × Type))
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
