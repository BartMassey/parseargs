-- Copyright © 2010 Bart Massey
-- This program is licensed under the "3-clause ('new') BSD License".
-- Please see the file COPYING in this distribution for license terms.

module Main
where

import Control.Monad
import Data.Maybe

import System.Console.ParseArgs

data Options =
    OptionFlag |
    OptionFlagInt |
    OptionFlagString |
    OptionPreoptional |
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
         Arg { argIndex = OptionPreoptional,
               argName = Nothing,
               argAbbr = Nothing,
               argData = argDataOptional "pre-optional" ArgtypeString,
               argDesc = "Test optional string before fixed" },
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

main :: IO ()
main = do
  args <- parseArgsIO 
            (ArgsParseControl (ArgsTrailing "junk") ArgsSoftDash) 
            argd
  putStrLn "parse successful"
  when (gotArg args OptionFlag)
       (putStrLn "saw flag")
  case (getArg args OptionFlagString) of
    Just s -> putStrLn ("saw string " ++ s)
    Nothing -> return ()
  case (getArg args OptionFlagInt) of
    Just d -> putStrLn ("saw int " ++ (show (d::Int)))
    Nothing -> return ()
  case (getArg args OptionPreoptional) of
    Just s -> putStrLn ("saw pre-optional " ++ s)
    Nothing -> return ()
  putStrLn ("saw fixed " ++ (fromJust (getArgString args OptionFixed)))
  case (getArg args OptionOptional) of
    Just s -> putStrLn ("saw optional " ++ s)
    Nothing -> return ()
  putStrLn ("saw rest: " ++ show (argsRest args))
