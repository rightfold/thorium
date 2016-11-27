module Thorium.Syntax
( Statement(..)
, Expression(..)
, Clause(..)
, Type(..)
) where

import Data.Generic (class Generic, gShow)
import Thorium.Prelude

data Statement
    = CreateInputStream String Type
    | CreateOutputStream String Type
    | CreateReactor String Expression

data Expression
    = Variable String
    | ClauseList (List Clause)

data Clause
    = From String String
    | Where Expression
    | SelectIntoInputStream Expression String
    | SelectIntoOutputStream Expression String

data Type
    = Record (List (String × Type))
    | Text
    | UUID

derive instance genericStatement  :: Generic Statement
derive instance genericExpression :: Generic Expression
derive instance genericClause     :: Generic Clause
derive instance genericType       :: Generic Type

instance showStatement  :: Show Statement  where show = gShow
instance showExpression :: Show Expression where show = gShow
instance showClause     :: Show Clause     where show = gShow
instance showType       :: Show Type       where show = gShow
