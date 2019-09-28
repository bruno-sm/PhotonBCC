module Ast
    ( Ast(..)
    , Definition(..)
    , Location(..)
    , Info(..)
    , Rule(..)
    , LocalVariable(..)
    , GenArgument(..)
    , SceneIdArgument(..)
    , SceneArgument(..)
    , NaturalArgument(..)
    , NumericArgument(..)
    ) where


data Location = Location { start_pos :: (Int, Int)
                         , end_pos :: (Int, Int)
                         , file :: String } deriving (Show)

data Info = Info { location :: Location } deriving (Show)

data Ast = Ast Info [Definition] deriving (Show)

data Definition = Definition { defInfo :: Info,
                               defName :: String,
                               defParameters :: [String],
                               defRules :: [Rule] }
                deriving (Show)

data Rule = Scene Info LocalVariable SceneIdArgument [GenArgument]
          | Paint Info SceneArgument 
          | Clear Info SceneArgument 
          | Param Info LocalVariable SceneArgument NaturalArgument 
          | Wait Info NumericArgument
          deriving (Show)

data LocalVariable = LocalVariable Info Integer deriving (Show)

data GenArgument = GenArgumentParameter Info String
                 | GenArgumentGlobalVariable Info String
                 | GenArgumentLocalVariable Info Integer
                 | GenArgumentNatural Info Integer
                 | GenArgumentInteger Info Integer
                 | GenArgumentReal Info Double 
                 | GenArgumentString Info String
                 | GenArgumentColor Info String
                 deriving (Show)

data SceneIdArgument = SceneIdGlobalVariable Info String
                     deriving (Show)

data SceneArgument = SceneArgumentLocalVariable Info Integer
                   deriving (Show)

data NumericArgument = NumericParameter Info String
                     | NumericLocalVariable Info Integer
                     | NumericLiteral Info Double
                     deriving (Show)

data NaturalArgument = NaturalParameter Info String
                     | NaturalLocalVariable Info Integer
                     | NaturalLiteral Info Integer 
                     deriving (Show)
