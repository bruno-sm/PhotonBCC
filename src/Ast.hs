module Ast
    ( Ast(..)
    , Definition(..)
    , Location(..)
    , Info(..)
    , Rule(..)
    , Variable(..)
    , GenParameter(..)
    , VarParameter(..)
    , NatParameter(..)
    , IntParameter(..)
    , NumParameter(..)
    , Type(..)
    ) where


data Location = Location { start_pos :: (Int, Int)
                         , end_pos :: (Int, Int)
                         , file :: String } deriving (Show)


data Info = Info { location :: Location } deriving (Show)

data Ast = Ast Info [Definition] deriving (Show)

data Definition = Definition {defInfo :: Info,
                              defName :: String,
                              defArgs :: [String],
                              defRules :: [Rule]}
                deriving (Show)

data Rule = Scene Info Variable String [GenParameter]
          | Paint Info VarParameter 
          | Clear Info VarParameter 
          | Param Info Variable VarParameter NatParameter 
          | Wait Info NumParameter 
          deriving (Show)

data Variable = Variable Info Integer deriving (Show)

data GenParameter = GenParamName Info String
                  | GenParamVar Info Integer
                  | GenParamNat Info Integer
                  | GenParamInt Info Integer
                  | GenParamReal Info Double 
                  | GenParamString Info String
                  | GenParamColor Info String
                  deriving (Show)

data VarParameter = VarParamName Info String
                  | VarParamVar Info Integer
                  deriving (Show)

data NumParameter = NumParamName Info String
                  | NumParamVar Info Integer
                  | NumParamReal Info Double 
                  deriving (Show)

data IntParameter = IntParamName Info String
                  | IntParamVar Info Integer
                  | IntParamInt Info Integer 
                  deriving (Show)

data NatParameter = NatParamName Info String
                  | NatParamVar Info Integer
                  | NatParamNat Info Integer 
                  deriving (Show)

data Type = TypeNat
          | TypeInt
          | TypeReal
          | TypeString
          | TypeColor
          | TypeAny
          | TypeScene [Type]
          deriving (Show)
