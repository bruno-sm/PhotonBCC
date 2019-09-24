module Syntax 
    ( Syntax.parse 
    ) where

import Debug.Trace (trace)
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Token
import Text.Parsec.Language
import System.IO
import Ast


position_info :: SourcePos -> SourcePos -> Info
position_info spos epos =
  Info { location = Location { start_pos = (sourceLine spos, sourceColumn spos)
                             , end_pos = (sourceLine epos, sourceColumn epos)
                             , file = sourceName spos} }


comment = string "/*" >> manyTill anyChar (try (string "*/")) >> return () <?> "comment"

white_char = char ' ' <|> char '\t'

opt_white = many white_char >> return () <?> "optional white space"

white = white_char >> opt_white <?> "white space"

gen_spaces = many ((space >> return ()) <|> comment)

nat :: Parser Integer 
nat = do
  i <- many1 digit
  return $ read i
  <?> "natural"

int_lit :: Parser Integer 
int_lit = do
  s <- optionMaybe (char '-')
  i <- many1 digit
  let sign = maybe 1 (\_ -> -1) s
  return $ (sign * read i)
  <?> "integer"

real :: Parser Double 
real = do
  s <- optionMaybe (char '-')
  i <- int_lit
  char '.'
  d <- int_lit
  let di = fromInteger i
      dd = fromInteger d
      sign = maybe 1 (\_ -> -1) s
  return (sign * (di + (dd * 0.1))) <?> "real"

number :: Parser Double 
number =
  try real <|> (int_lit >>= \i -> return $ fromInteger i) <?> "number"

name = do
  first <- noneOf [' ', '\t', '\"', '%', '\n', '\t', '\r', '#', '-', '@',
                   '0', '1', '2', '3', '4', '5', '6', '7', '8', '9'] 
  rest <- many (noneOf [' ', '\t', '\"', '\n', '\t', '\r']) 
  return (first:rest)
  <?> "name"

photonbc = do
  spos <- getPosition
  defs <- many (gen_spaces *> definition)
  gen_spaces
  epos <- getPosition
  eof
  pure $ Ast (position_info spos epos) defs 

getPos x = do
  p <- getPosition
  return $! p

definition = do
  spos <- getPosition
  string "def"
  white 
  n <- name
  args <- many (white *> name)
  epos <- getPosition
  opt_white
  endOfLine
  gen_spaces
  rules <- many (rule <* gen_spaces)
  pure $ Definition (position_info spos epos) n args rules 
  <?> "definition"

rule =
  scene
  <|> paint
  <|> clear
  <|> param
  <|> wait
  <?> "rule"
  where
    scene = do
      spos <- getPosition
      string "scene"
      white
      v <- localVariable (\i d -> LocalVariable i d)
      white
      id <- sceneIdArgument 
      params <- many (white >> genArgument)
      epos <- getPosition
      return $ Scene (position_info spos epos) v id params
      <?> "scene rule"
    paint = do
      spos <- getPosition
      string "paint"
      white
      s <- sceneArgument 
      epos <- getPosition
      return $ Paint (position_info spos epos) s
      <?> "paint rule"
    clear = do
      spos <- getPosition
      string "clear"
      white
      s <- sceneArgument 
      epos <- getPosition
      return $ Clear (position_info spos epos) s
      <?> "clear rule"
    param = do
      spos <- getPosition
      string "param"
      white
      v <- localVariable (\i d -> LocalVariable i d)
      white
      s <- sceneArgument 
      white
      i <- naturalArgument
      epos <- getPosition
      return $ Param (position_info spos epos) v s i
      <?> "arg rule"
    wait = do
      spos <- getPosition
      string "wait"
      white
      x <- numericArgument
      epos <- getPosition
      return $ Wait (position_info spos epos) x 
      <?> "wait rule"

genArgument =
  parameter (\i s -> GenArgumentParameter i s)
  <|> globalVariable (\i s -> GenArgumentGlobalVariable i s)
  <|> localVariable (\i n -> GenArgumentLocalVariable i n)
  <|> try (realNumber (\i x -> GenArgumentReal i x))
  <|> try (integerNumber (\i n -> GenArgumentInteger i n))
  <|> naturalNumber (\i n -> GenArgumentNatural i n)
  <|> pstring (\i s -> GenArgumentString i s)
  <|> color (\i c -> GenArgumentColor i c)
  <?> "general argument (prameter, global variable, local variable, number, string or color)"

sceneIdArgument =
  globalVariable (\i s -> SceneIdGlobalVariable i s)
  <|> parameter (\i s -> SceneIdParameter i s)

sceneArgument =
  parameter (\i s -> SceneArgumentParameter i s)
  <|> localVariable (\i n -> SceneArgumentLocalVariable i n)
  <?> "parameter or local variable"

numericArgument =
  parameter (\i s -> NumericParameter i s)
  <|> localVariable (\i n -> NumericLocalVariable i n)
  <|> anyNumber (\i x -> NumericLiteral i x)
  <?> "name, variable or number"

naturalArgument =
  parameter (\i s -> NaturalParameter i s)
  <|> localVariable (\i n -> NaturalLocalVariable i n)
  <|> naturalNumber (\i x -> NaturalLiteral i x)
  <?> "name, variable or natural number"

globalVariable f = do
  spos <- getPosition
  char '@'
  n <- name
  epos <- getPosition
  return $ f (position_info spos epos) n

parameter f = do
  spos <- getPosition
  n <- name
  epos <- getPosition
  return $ f (position_info spos epos) n

localVariable f = do
  spos <- getPosition
  char '%'
  i <- nat
  epos <- getPosition
  return $ f (position_info spos epos) i

naturalNumber f = do
  spos <- getPosition
  i <- nat
  epos <- getPosition
  return $ f (position_info spos epos) i

integerNumber f = do
  spos <- getPosition
  i <- int_lit 
  epos <- getPosition
  return $ f (position_info spos epos) i

realNumber f = do
  spos <- getPosition
  x <- real 
  epos <- getPosition
  return $ f (position_info spos epos) x

anyNumber f = do
  spos <- getPosition
  x <- number 
  epos <- getPosition
  return $ f (position_info spos epos) x

pstring f = do
  spos <- getPosition
  s <- stringLiteral $ makeTokenParser haskellDef
  epos <- getPosition
  return $ f (position_info spos epos) s

color f = do
  spos <- getPosition
  c <- color_literal
  epos <- getPosition
  return $ f (position_info spos epos) c 

color_literal :: Parser String
color_literal =
  char '#' >> count 8 (oneOf ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
                              'a', 'A', 'b', 'B', 'c', 'C', 'd', 'D', 'f'])
  <?> "hexadecimal color"


parse :: String -> IO (Either String Ast)
parse fpath = do
  res <- parseFromFile photonbc fpath
  return $ either (\err -> Left (show err)) (\ast -> Right ast) res
