--- Full-featured argument parser
--- Bart Massey 2007/01/06
---
--- Copyright (C) 2007 Bart Massey
--- ALL RIGHTS RESERVED

-- | This module supplies an argument parser parseArgs.
-- | Given a description of type [Arg] of the legal
-- | arguments to the program, a list of argument strings,
-- | and a bit of extra information, parseArgs returns an
-- | Args data structure suitable for querying using the
-- | provided functions gotArg, getArgString, etc.
module ParseArgs (Argtype(..), DataArg(..), Arg(..),
                  ArgsComplete(..),
                  ArgRecord, Args(..),
                  baseName, parseArgs,
                  gotArg, getArgString, getArgInteger, getArgInt,
                  getArgDouble, getArgFloat)
where

import Data.List
import qualified Data.Map as Map
import Control.Monad
import System.IO
import Data.Maybe
import System.Environment

--- The main job of this module is to provide parseArgs.
--- See below for its contract.

---
--- provided datatypes
---

--- The types of arguments carrying data;
--- the constructor arguments are for default values
data Argtype = ArgtypeString (Maybe String)
             | ArgtypeInteger (Maybe Integer)
             | ArgtypeInt (Maybe Int)
             | ArgtypeDouble (Maybe Double)
             | ArgtypeFloat (Maybe Float)

--- Information specific to an argument carrying data
data DataArg = DataArg { dataArgName :: String,
                         dataArgArgtype :: Argtype,
                         dataArgOptional :: Bool }

--- The description of an argument, suitable for
--- messages and for parsing.
---
--- XXX ugly representation
--- There are three cases:
---     1) The argument is a flag, in which case at least
---     one of argAbbr and argName is provided.
---        1.1) The flag itself requires an argument, in
---        which case argData is also provided.
---     2) The argument is positional, in which case neither
---     argAbbr nor argName are provided, but argData is
---     provided.
--- If none of argAbbr, argName, argData are provided, this
--- is an error.
--- In case 1.1 or 2, the dataArgOptional field of
--- argData determines whether the given flag or
--- positional argument must appear.  Obviously, all
--- flags are optional.

data (Ord a) => Arg a =
    Arg { argIndex :: a,
          argAbbr :: Maybe Char,
          argName :: Maybe String,
          argData :: Maybe DataArg,
          argDesc :: String }

---
--- returned datatypes
---

--- The kinds of "value" an argument can have.
data Argval = ArgvalFlag
            | ArgvalString String
            | ArgvalInteger Integer
            | ArgvalInt Int
            | ArgvalDouble Double
            | ArgvalFloat Float

type ArgRecord a = Map.Map a Argval

--- The data structure parseArgs produces.
data (Ord a) => Args a =
    Args { args :: ArgRecord a,
           argsProgName :: String,
           argsUsage :: String,
           argsRest :: [ String ] }

---
--- Implementation
---

--- Return the filename part of 'path'.
--- Unnecessarily efficient implementation does a single
--- tail-call traversal with no construction.
baseName :: String -> String
baseName s =
    let s' = dropWhile (/= '/') s in
    if null s' then s else baseName (tail s')


--- True if the described argument is positional
arg_posn :: (Ord a) => Arg a -> Bool
arg_posn (Arg { argAbbr = Nothing,
                argName = Nothing }) = True
arg_posn _ = False

--- True if the described argument is a flag
arg_flag :: (Ord a) => Arg a -> Bool
arg_flag a = not (arg_posn a)

--- True if the described argument is optional
arg_optional :: (Ord a) => Arg a -> Bool
arg_optional (Arg { argData = Just (DataArg { dataArgOptional = b }) }) = b
arg_optional _ = True

--- There's probably a better way to do this
perhaps b s = if b then s else ""

--- Format the described argument as a string
arg_string :: (Ord a) => Arg a -> String
arg_string a@(Arg { argAbbr = abbr,
                    argName = name,
                    argData = arg }) =
               (optionally "[") ++
               (sometimes flag_abbr abbr) ++
               (perhaps ((isJust abbr) && (isJust name)) ",") ++
               (sometimes flag_name name) ++
               (perhaps ((arg_flag a) && (isJust arg)) " ") ++
               (sometimes data_arg arg) ++
               (optionally "]")
    where
      sometimes = maybe ""
      optionally s = perhaps (arg_optional a) s
      flag_name s = "--" ++ s
      flag_abbr c = [ '-', c ]
      data_arg (DataArg {dataArgName = s}) = "<" ++ s ++ ">"

--- Filter out the empty keys for a hash.
filter_keys :: [ (Maybe a, b) ] -> [ (a, b) ]
filter_keys l =
    foldr check_key [] l
    where
      check_key (Nothing, _) rest = rest
      check_key (Just k, v) rest = (k, v) : rest

--- Fail with an error if the argument description is bad
--- for some reason.
argdesc_error :: String -> IO a
argdesc_error msg =
    error ("internal error: argument description: " ++ msg)

--- Make a keymap.
keymap_from_list :: (Ord k, Show k) =>
                    [ (k, a) ] -> IO (Map.Map k a)
keymap_from_list l =
    foldM add_entry Map.empty l
    where
      add_entry m (k, a) = 
          case Map.member k m of
            False -> return (Map.insert k a m)
            True -> argdesc_error ("duplicate argument description name " ++
                                   (show k))

--- Make a keymap for looking up a flag argument.
make_keymap :: (Ord a, Ord k, Show k) =>
               ((Arg a) -> Maybe k) ->
               [ Arg a ] ->
               IO (Map.Map k (Arg a))
make_keymap f_field args =
    (keymap_from_list .
     filter_keys .
     map (\arg -> (f_field arg, arg))) args

--- What can be left over after the parse?
data ArgsComplete =
    ArgsComplete |   --- no extraneous arguments
    ArgsTrailing |   --- trailing extraneous arguments OK
    ArgsInterspersed   --- any extraneous arguments OK

--- The function f is given a state s and a list [e], and expected
--- to produce a new state and a shorter list.  completeIO iterates
--- f until l is empty and returns the final state.
completeM :: (Monad m) => (s -> [e] -> m ([e], s)) -> s -> [e] -> m s
completeM f s [] = return s
completeM f s l = do
  (l', s') <- f s l
  completeM f s' l'

--- XXX Hooray for restricted polymorphism!
--- Print an error message during parsing.
parse_error :: String -> String -> IO a
parse_error usage msg = do
  hPutStrLn stderr usage
  error msg

--- Given a description of the arguments, parseArgs produces
--- a map from the arguments to their "values" and some other
--- useful byproducts.  Sadly, we're trapped in the IO monad
--- by wanting to report usage errors.
parseArgs :: (Show a, Ord a) =>
             ArgsComplete -> [ Arg a ] -> [ String ] ->
             IO (Args a)
parseArgs complete argd argv = do
  check_argd
  let flag_args = takeWhile arg_flag argd
  let posn_args = dropWhile arg_flag argd
  name_hash <- make_keymap argName flag_args
  abbr_hash <- make_keymap argAbbr flag_args
  pathname <- getProgName
  let prog_name = baseName pathname
  let usage = make_usage_string prog_name
  (am, posn, rest) <- completeM (parse usage name_hash abbr_hash)
                                (Map.empty, posn_args, [])
                                argv
  let required_args = filter (not . arg_optional) argd
  mapM_ (check_present usage am) required_args
  return (Args { args = am,
                 argsProgName = prog_name,
                 argsUsage = usage,
                 argsRest = rest })
  where
    check_present usage am ad@(Arg { argIndex = k }) =
        case Map.lookup k am of
          Just _ -> return ()
          Nothing -> parse_error usage ("missing required argument " ++
                                        (arg_string ad))
    --- Check for various possible misuses.
    check_argd = do
      --- Order must be flags, posn args, optional posn args
      let residue = dropWhile arg_flag argd
      let residue' = dropWhile arg_fixed_posn residue
      let residue'' = dropWhile arg_opt_posn residue'
      unless (null residue'')
             (argdesc_error "argument description in wrong order")
      --- No argument may be "nullary".
      when (or (map arg_nullary argd))
           (argdesc_error "bogus 'nothing' argument")
      where
        arg_fixed_posn a = (arg_posn a) && (not (arg_optional a))
        arg_opt_posn a = (arg_posn a) && (arg_optional a)
        arg_nullary (Arg { argName = Nothing,
                           argAbbr = Nothing,
                           argData = Nothing }) = True
        arg_nullary _ = False
    --- Generate a usage message string
    make_usage_string prog_name =
      --- top (summary) line
      (prog_name ++ ": usage: " ++ prog_name) ++
      (perhaps (not (null flag_args))
               " [options]") ++
      (perhaps (not (null posn_args))
               (" " ++ (unwords (map arg_string posn_args)))) ++
      (case complete of
         ArgsComplete -> ""
         _ -> " [--] ...") ++
      "\n" ++
      --- argument lines
      (concatMap (arg_line n) argd)
      where
        flag_args = filter arg_flag argd
        posn_args = filter arg_posn argd
        n = maximum (map (length . arg_string) argd)
        arg_line n a =
          let s = arg_string a in
            "  " ++ s ++ 
            (replicate (n - (length s)) ' ') ++
            "  " ++ (argDesc a) ++ "\n"
    --- simple recursive-descent parser
    parse _ _ _ av@(_, _, []) [] = return ([], av)
    parse usage _ _ av [] =
        case complete of
          ArgsComplete -> parse_error usage "unexpected extra arguments"
          _ -> return ([], av)
    parse usage name_hash abbr_hash (am, posn, rest) av@(aa : aas) =
        case aa of
          "--" -> case complete of
                    ArgsComplete -> parse_error usage
                                      ("unexpected -- " ++
                                      "(extra arguments not allowed)")
                    _ -> return ([], (am, posn, (rest ++ aas)))
          s@('-' : '-' : name) ->
              case Map.lookup name name_hash of
                Just ad -> peel s ad aas
                Nothing ->
                    case complete of
                      ArgsInterspersed ->
                          return (aas, (am, posn, rest ++ ["--" ++ name]))
                      _ -> parse_error usage
                           ("unknown argument --" ++ name)
          ('-' : abbr : abbrs) ->
              case Map.lookup abbr abbr_hash of
                Just ad -> do
                  p@(args', state') <- peel ['-', abbr] ad aas
                  case abbrs of
                    [] -> return p
                    ('-' : _) -> parse_error usage
                                 ("bad internal '-' in argument " ++ aa)
                    _ -> return (['-' : abbrs] ++ args', state')
                Nothing ->
                    case complete of
                      ArgsInterspersed ->
                          return (['-' : abbrs] ++ aas,
                                  (am, posn, rest ++ [['-', abbr]]))
                      _ -> parse_error usage
                           ("unknown argument -" ++ [abbr])
          aa -> case posn of
                  (ad@(Arg { argData = Just adata }) : ps) -> do
                          (argl', (am', _, rest')) <-
                              peel_process (dataArgName adata) ad av
                          return (argl', (am', ps, rest'))
                  [] -> case complete of
                          ArgsComplete -> parse_error usage
                                          ("unexpected argument " ++ aa)
                          _ -> return (aas, (am, [], rest ++ [aa]))
        where
          add_entry s m (k, a) =
              case Map.member k m of
                False -> return (Map.insert k a m)
                True -> parse_error usage ("duplicate argument " ++ s)
          peel name ad@(Arg { argData = Nothing, argIndex = index }) argl = do
              am' <- add_entry name am (index, ArgvalFlag)
              return (argl, (am', posn, rest))
          peel name (Arg { argData = Just (DataArg {}) }) [] =
              parse_error usage (name ++ " is missing its argument")
          peel name ad argl = peel_process name ad argl
          peel_process name
               ad@(Arg { argData = Just (DataArg {
                                     dataArgArgtype = atype }),
                         argIndex = index })
               (a : argl) = do
                 let v = case atype of
                           ArgtypeString _ -> ArgvalString a
                           ArgtypeInteger _ -> ArgvalInteger (read a)
                           ArgtypeInt _ -> ArgvalInt (read a)
                           ArgtypeDouble _ -> ArgvalDouble (read a)
                           ArgtypeFloat _ -> ArgvalFloat (read a)
                 am' <- add_entry name am (index, v)
                 return (argl, (am', posn, rest))

--- True if the arg was present.  Works on all types
gotArg :: (Ord a) => Args a -> a -> Bool
gotArg (Args { args = am }) k =
    case Map.lookup k am of
      Just _ -> True
      Nothing -> False

--- return the String, if any, of the given argument
getArgString :: (Show a, Ord a) => Args a -> a -> Maybe String
getArgString (Args { args = am }) k =
    case Map.lookup k am of
      Just (ArgvalString s) -> Just s
      Nothing -> Nothing
      _ -> error ("internal error: getArgString " ++ (show k))

--- return the Integer, if any, of the given argument
getArgInteger :: (Show a, Ord a) => Args a -> a -> Maybe Integer
getArgInteger (Args { args = am }) k =
    case Map.lookup k am of
      Just (ArgvalInteger s) -> Just s
      Nothing -> Nothing
      _ -> error ("internal error: getArgInteger " ++ (show k))

--- return the Int, if any, of the given argument
getArgInt :: (Show a, Ord a) => Args a -> a -> Maybe Int
getArgInt (Args { args = am }) k =
    case Map.lookup k am of
      Just (ArgvalInt s) -> Just s
      Nothing -> Nothing
      _ -> error ("internal error: getArgInt " ++ (show k))

--- return the Double, if any, of the given argument
getArgDouble :: (Show a, Ord a) => Args a -> a -> Maybe Double
getArgDouble (Args { args = am }) k =
    case Map.lookup k am of
      Just (ArgvalDouble s) -> Just s
      Nothing -> Nothing
      _ -> error ("internal error: getArgDouble " ++ (show k))

--- return the Float, if any, of the given argument
getArgFloat :: (Show a, Ord a) => Args a -> a -> Maybe Float
getArgFloat (Args { args = am }) k =
    case Map.lookup k am of
      Just (ArgvalFloat s) -> Just s
      Nothing -> Nothing
      _ -> error ("internal error: getArgFloat " ++ (show k))

