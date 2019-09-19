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
  first <- noneOf [' ', '\t', '\"', '%', '\n', '\t', '\r', '#', '-',
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
      v <- variable
      white
      n <- varparameter 
      params <- many (white >> genparameter)
      epos <- getPosition
      return $ Scene (position_info spos epos) v n params
      <?> "scene rule"
    paint = do
      spos <- getPosition
      string "paint"
      white
      v <- varparameter 
      epos <- getPosition
      return $ Paint (position_info spos epos) v
      <?> "paint rule"
    clear = do
      spos <- getPosition
      string "clear"
      white
      v <- varparameter 
      epos <- getPosition
      return $ Clear (position_info spos epos) v
      <?> "clear rule"
    param = do
      spos <- getPosition
      string "param"
      white
      v <- variable
      white
      s <- varparameter 
      white
      i <- natparameter
      epos <- getPosition
      return $ Param (position_info spos epos) v s i
      <?> "arg rule"
    wait = do
      spos <- getPosition
      string "wait"
      white
      x <- numparameter 
      epos <- getPosition
      return $ Wait (position_info spos epos) x 
      <?> "wait rule"

variable = do
  spos <- getPosition
  char '%' 
  i <- nat
  epos <- getPosition
  return $ Variable (position_info spos epos) i
  <?> "variable"

genparameter = do
  paramname (\i s -> GenParamName i s)
  <|> paramvar (\i n -> GenParamVar i n)
  <|> try (paramreal (\i x -> GenParamReal i x))
  <|> try (paramint (\i n -> GenParamInt i n))
  <|> paramnat (\i n -> GenParamNat i n)
  <|> paramstring (\i s -> GenParamString i s)
  <|> paramcolor (\i c -> GenParamColor i c)
  <?> "parameter (variable, number, string or color)"

varparameter =
  paramname (\i s -> VarParamName i s)
  <|> paramvar (\i n -> VarParamVar i n)
  <?> "name or variable"

numparameter =
  paramname (\i s -> NumParamName i s)
  <|> paramvar (\i n -> NumParamVar i n)
  <|> paramreal (\i x -> NumParamReal i x)
  <?> "name, variable or number"

intparameter =
  paramname (\i s -> IntParamName i s)
  <|> paramvar (\i n -> IntParamVar i n)
  <|> paramint (\i x -> IntParamInt i x)
  <?> "name, variable or integer number"

natparameter =
  paramname (\i s -> NatParamName i s)
  <|> paramvar (\i n -> NatParamVar i n)
  <|> paramnat (\i x -> NatParamNat i x)
  <?> "name, variable or natural number"

paramname f = do
  spos <- getPosition
  n <- name
  epos <- getPosition
  return $ f (position_info spos epos) n

paramvar f = do
  spos <- getPosition
  char '%'
  i <- nat
  epos <- getPosition
  return $ f (position_info spos epos) i

paramnat f = do
  spos <- getPosition
  i <- nat
  epos <- getPosition
  return $ f (position_info spos epos) i

paramint f = do
  spos <- getPosition
  i <- int_lit 
  epos <- getPosition
  return $ f (position_info spos epos) i

paramreal f = do
  spos <- getPosition
  x <- real 
  epos <- getPosition
  return $ f (position_info spos epos) x

paramstring f = do
  spos <- getPosition
  s <- string_literal 
  epos <- getPosition
  return $ f (position_info spos epos) s

paramcolor f = do
  spos <- getPosition
  c <- color 
  epos <- getPosition
  return $ f (position_info spos epos) c 

string_literal = stringLiteral $ makeTokenParser haskellDef

color :: Parser String
color =
  char '#' >> count 8 (oneOf ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
                              'a', 'A', 'b', 'B', 'c', 'C', 'd', 'D', 'f'])
  <?> "hexadecimal color"


parse :: String -> IO (Either String Ast)
parse fpath = do
  res <- parseFromFile photonbc fpath
  return $ either (\err -> Left (show err)) (\ast -> Right ast) res
