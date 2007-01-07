module Main
where

import ParseArgs
import System.Environment

data Options = OptBogus deriving (Ord, Eq)

argd :: ArgDesc Options
argd = ArgDesc { argDescArgs = [],
                 argDescPosnArgs = [],
                 argDescOptArgs = [],
                 argDescComplete = True }

main = do
  argv <- getArgs
  args <- parseArgs argd argv
  putStrLn "parse successful"
