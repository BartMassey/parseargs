module Main
where

import ParseArgs
import System.Environment

data Options =
    OptionFlag |
    OptionFlagInt |
    OptionFixed |
    OptionOptional
    deriving (Ord, Eq)

argd :: [ Arg Options ]
argd = [ Arg { argIndex = OptionFlag,
               argName = Just "flag",
               argAbbr = Just 'f',
               argData = Nothing,
               argDesc = "Test flag" },
         Arg { argIndex = OptionFlagInt,
               argName = Just "intflag",
               argAbbr = Nothing,
               argData = Just (DataArg { dataArgName = "test-value",
                                         dataArgArgtype = ArgtypeInt,
                                         dataArgOptional = True }),
               argDesc = "Test int flag" },
         Arg { argIndex = OptionFixed,
               argName = Nothing,
               argAbbr = Nothing,
               argData = Just (DataArg { dataArgName = "fixed",
                                         dataArgArgtype = ArgtypeString,
                                         dataArgOptional = False }),
               argDesc = "Test fixed string" },
         Arg { argIndex = OptionOptional,
               argName = Nothing,
               argAbbr = Nothing,
               argData = Just (DataArg { dataArgName = "optional",
                                         dataArgArgtype = ArgtypeString,
                                         dataArgOptional = True }),
               argDesc = "Test optional string" }]

main = do
  argv <- getArgs
  args <- parseArgs ArgsComplete argd argv
  putStrLn "parse successful"
