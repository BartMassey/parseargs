module Main
where

import ParseArgs
import System.Environment

data Options =
    OptionFlag |
    OptionFixed |
    OptionOptional
    deriving (Ord, Eq)

argd :: ArgDesc Options
argd = ArgDesc { argDescArgs = [Arg { argIndex = OptionFlag,
                                      argAtype = ArgAtypeFlag,
                                      argName = "flag",
                                      argAbbr = Just 'f',
                                      argExprName = Nothing,
                                      argDesc = "Test flag" }],
                 argDescPosnArgs = [Arg { argIndex = OptionFixed,
                                      argAtype = ArgAtypeString,
                                      argName = "fixed",
                                      argAbbr = Nothing,
                                      argExprName = Nothing,
                                      argDesc = "Test fixed argument" }],
                 argDescOptArgs = [Arg { argIndex = OptionOptional,
                                      argAtype = ArgAtypeString,
                                      argName = "optional",
                                      argAbbr = Nothing,
                                      argExprName = Nothing,
                                      argDesc = "Test optional argument" }],
                 argDescComplete = True }

main = do
  argv <- getArgs
  args <- parseArgs argd argv
  putStrLn "parse successful"
