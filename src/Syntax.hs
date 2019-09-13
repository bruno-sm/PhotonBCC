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
  Info { location = Location { start_pos = (sourceLine spos, sourceColumn epos)
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
  return $ Ast (position_info spos epos) defs 

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
  return $ Definition (position_info spos epos) n args rules 
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
      n <- name
      params <- many (white >> parameter)
      epos <- getPosition
      return $ Scene (position_info spos epos) v n params
      <?> "scene rule"
    paint = do
      spos <- getPosition
      string "paint"
      white
      v <- char '%' >> nat 
      epos <- getPosition
      return $ Paint (position_info spos epos) v
      <?> "paint rule"
    clear = do
      spos <- getPosition
      string "clear"
      white
      v <- char '%' >> nat 
      epos <- getPosition
      return $ Clear (position_info spos epos) v
      <?> "clear rule"
    param = do
      spos <- getPosition
      string "param"
      white
      v <- variable
      white
      s <- char '%' >> nat 
      white
      i <- nat
      epos <- getPosition
      return $ Param (position_info spos epos) v s i
      <?> "arg rule"
    wait = do
      spos <- getPosition
      string "wait"
      white
      x <- real
      epos <- getPosition
      return $ Wait (position_info spos epos) x 
      <?> "wait rule"

variable =
  getPosition >>= \spos ->
  char '%' >> 
  ((nat >>= \i -> getPosition >>= \epos ->
    return $ Variable (position_info spos epos) i)
  <|> (char '_' >> getPosition >>= \epos ->
    return $ EmptyVariable (position_info spos epos)))
  <?> "variable"

parameter = do
  paramname
  <|> paramvar
  <|> try paramreal
  <|> try paramint
  <|> paramnat
  <|> paramstring
  <|> paramcolor
  <?> "parameter (variable, number, string or color)"
  where
    paramname = do
      spos <- getPosition
      n <- name
      epos <- getPosition
      return $ ParamName (position_info spos epos) n
    paramvar = do
      spos <- getPosition
      char '%'
      i <- nat
      epos <- getPosition
      return $ ParamVar (position_info spos epos) i
    paramnat = do
      spos <- getPosition
      i <- nat
      epos <- getPosition
      return $ ParamNat (position_info spos epos) i
    paramint = do
      spos <- getPosition
      i <- int_lit 
      epos <- getPosition
      return $ ParamInt (position_info spos epos) i
    paramreal = do
      spos <- getPosition
      x <- real 
      epos <- getPosition
      return $ ParamReal (position_info spos epos) x
    paramstring = do
      spos <- getPosition
      s <- string_literal 
      epos <- getPosition
      return $ ParamString (position_info spos epos) s
    paramcolor = do
      spos <- getPosition
      c <- color 
      epos <- getPosition
      return $ ParamColor (position_info spos epos) c 


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
