module Ast
    ( Ast(..)
    , Definition(..)
    , Location(..)
    , Info(..)
    , Rule(..)
    , Variable(..)
    , Parameter(..)
    , Type(..)
    ) where


data Location = Location { start_pos :: (Int, Int)
                         , end_pos :: (Int, Int)
                         , file :: String } deriving (Show)


data Info = Info { location :: Location
                 , _type :: Type } deriving (Show)

data Ast = Ast Info [Definition] deriving (Show)

data Definition = Definition Info String [String] [Rule] deriving (Show)

data Rule = Scene Info Variable String [Parameter]
          | Paint Info Integer 
          | Clear Info Integer 
          | Param Info Variable Integer Integer
          | Wait Info Double 
          deriving (Show)

data Variable = Variable Info Integer | EmptyVariable Info deriving (Show)

data Parameter = ParamName Info String
               | ParamVar Info Integer
               | ParamNat Info Integer
               | ParamInt Info Integer
               | ParamReal Info Double 
               | ParamString Info String
               | ParamColor Info String
               deriving (Show)

data Type = TypeNat
          | TypeInt
          | TypeReal
          | TypeString
          | TypeColor
          | TypeAny
          | TypeScene [Type]
          deriving (Show)
