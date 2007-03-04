module Main
where

import ParseArgs
import System.Environment
import Control.Monad

data Options =
    OptionFlag |
    OptionFlagInt |
    OptionFlagString |
    OptionFixed |
    OptionOptional
    deriving (Ord, Eq, Show)

argd :: [ Arg Options ]
argd = [ Arg { argIndex = OptionFlag,
               argName = Just "flag",
               argAbbr = Just 'f',
               argData = Nothing,
               argDesc = "Test flag" },
         Arg { argIndex = OptionFlagString,
               argName = Just "string-flag",
               argAbbr = Just 's',
               argData = Just (DataArg { dataArgName = "test-value",
                                         dataArgArgtype = ArgtypeString Nothing,
                                         dataArgOptional = True }),
               argDesc = "Test string flag" },
         Arg { argIndex = OptionFlagInt,
               argName = Just "int-flag",
               argAbbr = Nothing,
               argData = Just (DataArg { dataArgName = "test-value",
                                         dataArgArgtype = ArgtypeInt (Just 7),
                                         dataArgOptional = True }),
               argDesc = "Test int flag" },
         Arg { argIndex = OptionFixed,
               argName = Nothing,
               argAbbr = Nothing,
               argData = Just (DataArg { dataArgName = "fixed",
                                         dataArgArgtype = ArgtypeString Nothing,
                                         dataArgOptional = False }),
               argDesc = "Test fixed string" },
         Arg { argIndex = OptionOptional,
               argName = Nothing,
               argAbbr = Nothing,
               argData = Just (DataArg { dataArgName = "optional",
                                         dataArgArgtype = ArgtypeString Nothing,
                                         dataArgOptional = True }),
               argDesc = "Test optional string" }]

main = do
  argv <- getArgs
  args <- parseArgs ArgsComplete argd argv
  putStrLn "parse successful"
  when (gotArg args OptionFlag)
       (putStrLn "saw flag")
  case (getArgString args OptionFlagString) of
    Just s -> putStrLn ("saw string " ++ s)
    Nothing -> return ()
  case (getArgInt args OptionFlagInt) of
    Just d -> putStrLn ("saw int " ++ (show d))
    Nothing -> return ()
