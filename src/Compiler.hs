module Compiler 
    ( compile 
    ) where

import Debug.Trace (trace)
import Syntax
import System.IO


compile :: [String] -> IO String
compile [] = return "" 
compile (hd:tl) =
  compile1 hd >>= \res ->
  case res of
  Nothing -> compile tl
  Just err -> return err 


compile1 :: String -> IO (Maybe String)
compile1 path = do
  res <- parse path
  return $ either (\err -> Just err) (\ast -> trace (show ast) Nothing) res
