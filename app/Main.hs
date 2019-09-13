{-# LANGUAGE QuasiQuotes #-}
module Main where

import Compiler 
import System.Environment (getArgs)
import System.Console.Docopt

patterns :: Docopt
patterns = [docoptFile|USAGE.txt|]

getArgOrExit = getArgOrExitWith patterns

main :: IO ()
main = do
  args <- parseArgsOrExit patterns =<< getArgs
  let files = getAllArgs args (argument "file")
  res <- compile files
  putStrLn res
