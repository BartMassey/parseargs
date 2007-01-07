--- Full-featured argument parser
--- Bart Massey 2007/01/06
---
--- Copyright (C) 2007 Bart Massey
--- ALL RIGHTS RESERVED

module ParseArgs (baseName, parseArgs)
where

import Data.List
import qualified Data.Map as Map

--- The main job of this module is to provide parseArgs.
--- See below for its contract.

--- The kinds of "value" an argument can have.
data ArgVal =
    ArgValFlag |
    ArgValString String |
    ArgValInt Int

--- The corresponding argument type requirements
data ArgAtype =
    ArgAtypeFlag |
    ArgAtypeString |
    ArgAtypeInt

--- The description of an argument, suitable for
--- messages and for parsing.
--- It is expected that each argument be identified
--- by an enumeration value, so that its value can easily
--- be referenced later.
data (Ord a) => Arg a =
    Arg { argIndex :: a,
          argAtype :: ArgAtype,
          argAbbr :: Maybe Char,
          argName :: Maybe String,
          argExprName :: Maybe String,
          argDesc :: String }

--- The full description of the parsing problem for
--- parseArgs to swallow.
data ArgDesc a =
    ArgDesc { argDescArgs :: [ Arg a ],
              argDescPosnArgs :: [ Arg a ],
              argDescOptArgs :: [ Arg a ],
              argDescComplete :: Bool }

--- The data structure parseArgs produces.
data (Ord a) => Args a =
    Args { args :: Map.Map a ArgVal,
           argsProgName :: String,
           argsRest :: [ String ] }

--- Return the filename part of 'path'.
--- Unnecessarily efficient implementation does a single
--- tail-call traversal with no construction.
baseName :: String -> String
baseName s =
    let s' = dropWhile (/= '/') s in
    if null s' then s else baseName (tail s')


--- Given an arg desc, produce a description string.
arg_string :: (Ord a) => Arg a -> String
arg_string (Arg { argAbbr = abbr,
                  argName = name,
                  argExprName = exprName }) =
    case exprName of
      Nothing -> flag_part ""
      Just s -> (flag_part " ") ++ "<" ++ s ++ ">"
    where
      flag_part :: String -> String
      flag_part sep =
      	  case name of
            Nothing -> ""
            Just s ->
                (case abbr of
                  Nothing -> ""
                  Just c -> "-" ++ [c] ++ ",") ++
                ("--" ++ s) ++
                sep

--- Filter out the empty keys for a hash.
filter_keys :: [ (Maybe a, b) ] -> [ (a, b) ]
filter_keys l =
    foldr check_key [] l
    where
      check_key :: (Maybe a, b) -> [ (a, b) ] -> [ (a, b) ]
      check_key (Nothing, _) rest = rest
      check_key (Just k, v) rest = (k, v) : rest

--- Given a description of the arguments, parseArgs produces
--- a map from the arguments to their "values" and some other
--- useful byproducts.  Sadly, we're trapped in the IO monad
--- by wanting to report usage errors.
parseArgs :: (Ord a) => ArgDesc a -> [ String ] -> IO (Args a)
parseArgs argd argv = do
  let ads = argDescArgs argd
  let abbr_hash = make_keymap argAbbr ads
  let name_hash = make_keymap argName ads
  return (Args { args = Map.empty, argsProgName = "", argsRest = [] })
  where
    make_keymap :: (Ord b, Ord c) =>
                   ((Arg b) -> Maybe c) ->
                   [ Arg b ] ->
                   Map.Map c (Arg b)
    make_keymap f_field args =
        (Map.fromList .
         filter_keys .
         map (\arg -> (f_field arg, arg)))
           args
