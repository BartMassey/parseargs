module Main
where

import System.Console.ParseArgs
import System.Environment
import Control.Monad
import Data.Maybe

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
               argData = argDataOptional "test-value" ArgtypeString,
               argDesc = "Test string flag" },
         Arg { argIndex = OptionFlagInt,
               argName = Just "int-flag",
               argAbbr = Nothing,
               argData = argDataDefaulted "test-value" ArgtypeInt 7,
               argDesc = "Test int flag" },
         Arg { argIndex = OptionFixed,
               argName = Nothing,
               argAbbr = Nothing,
               argData = argDataRequired "fixed" ArgtypeString,
               argDesc = "Test fixed string" },
         Arg { argIndex = OptionOptional,
               argName = Nothing,
               argAbbr = Nothing,
               argData = argDataOptional "optional" ArgtypeString,
               argDesc = "Test optional string" }]

main = do
  args <- parseArgsIO ArgsComplete argd
  putStrLn "parse successful"
  when (gotArg args OptionFlag)
       (putStrLn "saw flag")
  case (getArgString args OptionFlagString) of
    Just s -> putStrLn ("saw string " ++ s)
    Nothing -> return ()
  case (getArgInt args OptionFlagInt) of
    Just d -> putStrLn ("saw int " ++ (show d))
    Nothing -> return ()
  putStrLn ("saw fixed " ++ (fromJust (getArgString args OptionFixed)))
  case (getArgString args OptionOptional) of
    Just s -> putStrLn ("saw optional " ++ s)
    Nothing -> return ()