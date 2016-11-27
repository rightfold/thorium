module Thorium.Language.Syntax
( Statement(..)
, Expression(..)
, Clause(..)
, Type(..)
) where

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
