{-# LANGUAGE FlexibleInstances, DeriveDataTypeable, Safe, CPP #-}
------------------------------------------------------------
-- |
-- Module      :  System.Console.ParseArgs
-- Description :  Full-featured command-line argument parsing library.
-- Copyright   :  (c) 2007 Bart Massey
-- License     :  BSD-style (see the file COPYING)
-- Maintainer  :  Bart Massey <bart.massey@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
-- `ParseArgs` is a full-featured command-line argument
-- parsing library.
--
-- This module supplies an argument parser.  Given a
-- description of type [`Arg`] of the legal arguments to the
-- program, a list of argument strings, and a bit of extra
-- information, the `parseArgs` function in this module
-- returns an `Args` data structure suitable for querying
-- using the provided functions `gotArg`, `getArg`, etc.
------------------------------------------------------------

module System.Console.ParseArgs (
  -- * Describing allowed arguments
  -- |The argument parser requires a description of
  -- the arguments that will be parsed.  This is
  -- supplied as a list of `Arg` records, built up
  -- using the functions described here.
  Arg(..),
  Argtype(..), 
  ArgsComplete(..),
  ArgsDash(..),
  APCData(..),
  ArgsParseControl(..),
  -- ** DataArg and its pseudo-constructors
  DataArg,
  argDataRequired, argDataOptional, argDataDefaulted,
  -- * Argument processing
  -- |The argument descriptions are used to parse
  -- the command line arguments, and the results
  -- of the parse can later be (efficiently) queried
  -- to determine program behavior.

  -- ** Getting parse results
  -- |The argument parser returns an opaque map
  -- from argument index to parsed argument data
  -- (plus some convenience information).
  Args(..),
  parseArgs, parseArgsIO,
  -- ** Using parse results
  -- |Query functions permit checking for the existence
  -- and values of command-line arguments.
  gotArg, ArgType(..),
  getArgString, getArgFile, getArgStdio,
  getArgInteger, getArgInt,
  getArgDouble, getArgFloat,
  ArgFileOpener(..),
  -- * Misc
  ParseArgsException(..),
  baseName, parseError, usageError,
  System.IO.IOMode(ReadMode, WriteMode, AppendMode))
where

import Control.Exception
import Control.Monad
#if __GLASGOW_HASKELL__ < 710
import Control.Monad.ST.Safe
#else
import Control.Monad.ST
#endif
import Data.List
import qualified Data.Map as Map
import Data.Maybe
import Data.Typeable
import System.Environment
import System.IO

-- The main job of this module is to provide parseArgs.
-- See below for its contract.

--
-- Provided datatypes.
--

-- |The description of an argument, suitable for
-- messages and for parsing.  The `argData` field
-- is used both for flags with a data argument, and
-- for positional data arguments.
-- 
-- There are two cases:
--
--     (1) The argument is a flag, in which case at least
--     one of `argAbbr` and `argName` is provided;
--
--     (2) The argument is positional, in which case neither
--     `argAbbr` nor `argName` are provided, but `argData` is.
-- 
-- If none of `argAbbr`, `argName`, or `argData` are
-- provided, this is an error.  See also the
-- `argDataRequired`, `argDataOptional`, and
-- `argDataDefaulted` functions below, which are used to
-- generate `argData`.
data (Ord a) => Arg a =
    Arg { argIndex :: a              -- ^Connects the input description
                                     -- to the output argument.
        , argAbbr :: Maybe Char      -- ^One-character flag name.
        , argName :: Maybe String    -- ^\"Long name\" of flag.
        , argData :: Maybe DataArg   -- ^Datum description.
        , argDesc :: String          -- ^Documentation for the argument.
        } 


-- |The types of an argument carrying data.  The constructor
-- argument is used to carry a default value.
--
-- The constructor argument should really be hidden.
-- Values of this type are normally constructed within
-- the pseudo-constructors pseudo-constructors
-- `argDataRequired`, `argDataOptional`, and
-- `argDataDefaulted`, to which only the constructor
-- function itself is passed.
data Argtype = ArgtypeString (Maybe String)
             | ArgtypeInteger (Maybe Integer)
             | ArgtypeInt (Maybe Int)
             | ArgtypeDouble (Maybe Double)
             | ArgtypeFloat (Maybe Float)


-- |Information specific to an argument carrying a datum.  This
-- is an opaque type, whose instances are constructed using the
-- pseudo-constructors `argDataRequired`, `argDataOptional`,
-- and `argDataDefaulted`.
data DataArg = DataArg { dataArgName :: String       -- ^Print name of datum.
                       , dataArgArgtype :: Argtype   -- ^Type of datum.
                       , dataArgOptional :: Bool     -- ^Datum is not required.
                       }

-- |Generate the `argData` for the given non-optional argument.
argDataRequired :: String                 -- ^Datum print name.
                -> (Maybe a -> Argtype)   -- ^Type constructor for datum.
                -> Maybe DataArg          -- ^Result is `argData`-ready.
argDataRequired s c = Just (DataArg { dataArgName = s,
                                      dataArgArgtype = c Nothing,
                                      dataArgOptional = False })

-- |Generate the `argData` for the given optional argument with no default.
argDataOptional :: String                 -- ^Datum print name.
                -> (Maybe a -> Argtype)   -- ^Type constructor for datum.
                -> Maybe DataArg          -- ^Result is `argData`-ready.
argDataOptional s c = Just (DataArg { dataArgName = s,
                                      dataArgArgtype = c Nothing,
                                      dataArgOptional = True })

-- |Generate the `argData` for the given optional argument with the
-- given default.
argDataDefaulted :: String                 -- ^Datum print name.
                 -> (Maybe a -> Argtype)   -- ^Type constructor for datum.
                 -> a                      -- ^Datum default value.
                 -> Maybe DataArg          -- ^Result is `argData`-ready.
argDataDefaulted s c d = Just (DataArg { dataArgName = s,
                                         dataArgArgtype = c (Just d),
                                         dataArgOptional = True })
--
-- Returned datatypes.
--

-- |The \"kinds of values\" an argument can have.
data Argval = ArgvalFlag   -- ^For simple present vs not-present flags.
            | ArgvalString String
            | ArgvalInteger Integer
            | ArgvalInt Int
            | ArgvalDouble Double
            | ArgvalFloat Float

-- |The type of the mapping from argument index to value.
newtype ArgRecord a = ArgRecord (Map.Map a Argval)

-- |The data structure `parseArgs` produces. There is a should-be-hidden
-- field that describes the parse.
data (Ord a) => Args a =
    Args { __args :: ArgRecord a    -- ^The argument parse, only listed here
                                    -- to work around a Haddock bug. See
                                    -- <https://github.com/haskell/haddock/issues/456>.
         , argsProgName :: String   -- ^Basename of 0th argument.
         , argsUsage :: String      -- ^Full usage string.
         , argsRest :: [ String ]   -- ^Remaining unprocessed arguments.
         }

--
-- Exception type.
--

-- |This exception is raised with an appropriate error message
-- when argument parsing fails.  The first argument is the usage
-- message, the second the actual error message from the parser.
data ParseArgsException = ParseArgsException String String
     deriving (Eq, Typeable)

instance Exception ParseArgsException

instance Show ParseArgsException where
    show (ParseArgsException usage msg) = msg ++ "\n" ++ usage

--
-- Implementation.
--

-- |True if the described argument is positional.
arg_posn :: (Ord a) =>
            Arg a   -- ^Argument.
         -> Bool    -- ^True if argument is positional.
arg_posn (Arg { argAbbr = Nothing,
                argName = Nothing }) = True
arg_posn _ = False

-- |True if the described argument is a flag.
arg_flag :: (Ord a) =>
            Arg a   -- ^Argument.
         -> Bool    -- ^True if argument is a flag.
arg_flag a = not (arg_posn a)

-- |True if the described argument is optional.
arg_optional :: (Ord a) =>
                Arg a   -- ^Argument.
             -> Bool    -- ^False if argument is required to be present.
arg_optional (Arg { argData = Just (DataArg { dataArgOptional = b }) }) = b
arg_optional _ = True

arg_required :: (Ord a) =>
                Arg a   -- ^Argument.
             -> Bool    -- ^True if argument is required to be present.
arg_required a = not (arg_optional a)

-- |Return the value of a defaulted argument.
arg_default_value :: (Ord a)
                  => Arg a         -- ^Argument.
                  -> Maybe Argval  -- ^Optional default value.
arg_default_value arg@(Arg { argData = Just
                             (DataArg { dataArgArgtype = da }) }) |
                             arg_optional arg =
    defval da
    where
      defval (ArgtypeString (Just v)) = Just (ArgvalString v)
      defval (ArgtypeInteger (Just v)) = Just (ArgvalInteger v)
      defval (ArgtypeInt (Just v)) = Just (ArgvalInt v)
      defval (ArgtypeDouble (Just v)) = Just (ArgvalDouble v)
      defval (ArgtypeFloat (Just v)) = Just (ArgvalFloat v)
      defval _ = Nothing
arg_default_value _ = Nothing

-- |There's probably a better way to do this.
perhaps :: Bool -> String -> String
perhaps b s = if b then s else ""

-- |Format the described argument as a string.
arg_string :: (Ord a) =>
              Arg a    -- ^Argument to be described.
           -> String   -- ^String describing argument.
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

-- |Filter out the empty keys for a hash.
filter_keys :: [ (Maybe a, b) ]   -- ^List of (optional key, value) pairs.
            -> [ (a, b) ]         -- ^Pairs with actual keys.
filter_keys l =
    foldr check_key [] l
    where
      check_key (Nothing, _) rest = rest
      check_key (Just k, v) rest = (k, v) : rest

-- |Fail with an error if the argument description is bad
-- for some reason.
argdesc_error :: String   -- ^Error message.
              -> a        -- ^Bogus polymorphic result.
argdesc_error msg =
    error ("internal error: argument description: " ++ msg)

-- |Make a keymap.
keymap_from_list :: (Ord k, Show k) =>
                    [ (k, a) ]    -- ^List of key-value pairs.
                                  -- Will be checked for duplicate keys.
                 -> Map.Map k a   -- ^Key-value map.
keymap_from_list l =
    foldl add_entry Map.empty l
    where
      add_entry m (k, a) = 
          case Map.member k m of
            False -> Map.insert k a m
            True -> argdesc_error ("duplicate argument description name " ++
                                   (show k))

-- |Make a keymap for looking up a flag argument.
make_keymap :: (Ord a, Ord k, Show k) =>
               ((Arg a) -> Maybe k)   -- ^Mapping from argdesc to flag key.
            -> [ Arg a ]              -- ^List of argdesc.
            -> (Map.Map k (Arg a))    -- ^Map from key to argdesc.
make_keymap f_field ads =
    (keymap_from_list .
     filter_keys .
     map (\arg -> (f_field arg, arg))) ads

-- |How \"sloppy\" the parse is.
data ArgsComplete = ArgsComplete         -- ^Any extraneous arguments
                                         -- (unparseable from description)
                                         -- will cause the parser to fail.
                  | ArgsTrailing String  -- ^Trailing extraneous arguments are
                                         -- permitted, and will be skipped,
                                         -- saved, and returned.  The
                                         -- constructor argument is the
                                         -- name of the args.
                  | ArgsInterspersed     -- ^All extraneous arguments are
                                         -- permitted, and will be skipped,
                                         -- saved, and returned.

-- |Whether to always treat an unknown argument beginning
-- with \"-\" as an error, or to allow it to be used as a
-- positional argument when possible.
data ArgsDash = ArgsHardDash   -- ^If an argument begins with
                               -- a \"-\", it will always be
                               -- treated as an error unless
                               -- it corresponds to a flag description.
              | ArgsSoftDash   -- ^If an argument beginning with
                               -- a \"-\" is unrecognized as a flag,
                               -- treat it as a positional argument
                               -- if possible. Otherwise it is an error.
              deriving Eq

-- |Record containing the collective parse control information.
data ArgsParseControl = ArgsParseControl {
  -- |Level of completeness of parse.
  apcComplete :: ArgsComplete,
  -- |Handling of dashes in parse.
  apcDash :: ArgsDash }

-- |Class for building parse control information,
-- for backward compatibility.
class APCData a where
  getAPCData :: a -> ArgsParseControl  -- ^Build an 'ArgsParseControl'
                                       -- structure from the given info.

instance APCData ArgsParseControl where
  getAPCData a = a

instance APCData ArgsComplete where
  getAPCData a = ArgsParseControl a ArgsHardDash

-- |The iteration function is given a state and a list, and
-- expected to produce a new state and list.  The function
-- is again invoked with the resulting state and list.  When
-- the supplied function returns the empty list, this
-- function returns the final state produced.
exhaust :: (s -> [e] -> ([e], s))   -- ^Function to iterate.
        -> s                        -- ^Initial state.
        -> [e]                      -- ^Initial list.
        -> s                        -- ^Final state.
exhaust _ s [] = s
exhaust f s l =
  let (l', s') = f s l
  in exhaust f s' l'

-- |Generate a usage error with the given supplementary message string.
parseError :: String    -- ^Usage message.
            -> String    -- ^Specific error message.
            -> a         -- ^Bogus polymorphic result.
parseError usage msg =
  throw (ParseArgsException usage msg)

-- |Given a description of the arguments, `parseArgs`
-- produces a map from the arguments to their \"values\" and
-- some other useful byproducts.  `parseArgs` requires that
-- the argument descriptions occur in the order 1) flag
-- arguments, then 2) positional arguments; otherwise a
-- runtime error will be thrown.
parseArgs :: (Show a, Ord a, APCData b) =>
             b              -- ^Configuration for parse.
          -> [ Arg a ]      -- ^Argument descriptions.
          -> String         -- ^Full program pathname.
          -> [ String ]     -- ^Incoming program argument list.
          -> Args a         -- ^Outgoing argument parse results.
parseArgs apcData argd pathname argv =
  runST (do
           check_argd
           let (flag_args, posn_args) = span arg_flag argd
           let name_hash = make_keymap argName flag_args
           let abbr_hash = make_keymap argAbbr flag_args
           let prog_name = baseName pathname
           let usage = make_usage_string prog_name
           let (am, _, rest) = exhaust (parse usage name_hash abbr_hash)
                                (Map.empty, posn_args, [])
                                argv
           let required_args = filter (not . arg_optional) argd
           unless (and (map (check_present usage am) required_args))
                  (error "internal error")
           let am' = foldl supply_defaults am argd
           return (Args { __args = ArgRecord am',
                          argsProgName = prog_name,
                          argsUsage = usage,
                          argsRest = rest }))
  where
    supply_defaults am ad@(Arg { argIndex = k }) =
        case Map.lookup k am of
          Just _ -> am
          Nothing -> case arg_default_value ad of
                       Just v -> Map.insert k v am
                       Nothing -> am
    check_present usage am ad@(Arg { argIndex = k }) =
        case Map.lookup k am of
          Just _ -> True
          Nothing -> parseError usage ("missing required argument " ++
                                        (arg_string ad))
    --- Check for various possible misuses.
    check_argd :: ST s ()
    check_argd = do
      --- Order must be flags, then posn args
      let (_, posns) = span arg_flag argd
      unless (all arg_posn posns)
             (argdesc_error "argument description mixes flags and positionals")
      --- No argument may be "nullary".
      when (or (map arg_nullary argd))
           (argdesc_error "bogus 'nothing' argument")
      return ()
      where
        arg_nullary (Arg { argName = Nothing,
                           argAbbr = Nothing,
                           argData = Nothing }) = True
        arg_nullary _ = False
    --- Generate a usage message string
    make_usage_string prog_name =
      summary_line ++ arg_lines
      where
        flag_args = filter arg_flag argd
        posn_args = filter arg_posn argd
        n = maximum (map (length . arg_string) argd)
        --- top (summary) line
        summary_line = 
            "usage: " ++ prog_name ++
            perhaps
              (not (null flag_args))
              " [options]" ++
            perhaps
              (not (null posn_args))
              (" " ++ unwords (map arg_string posn_args)) ++
            (case apcComplete $ getAPCData apcData of
               ArgsComplete -> ""
               ArgsTrailing s -> " [--] [" ++ s ++ " ...]"
               ArgsInterspersed -> " ... [--] ...") ++ "\n"
        --- argument lines
        arg_lines = concatMap (arg_line n) argd where
            arg_line na a =
                let s = arg_string a in
                "  " ++ s ++ 
                replicate (na - (length s)) ' ' ++
                "  " ++ argDesc a ++ "\n"
    --- simple recursive-descent parser
    parse _ _ _ av@(_, _, []) [] = ([], av)
    parse usage _ _ av [] =
        case apcComplete $ getAPCData apcData of
          ArgsComplete -> parseError usage "unexpected extra arguments"
          _ -> ([], av)
    parse usage name_hash abbr_hash (am, posn, rest) av@(aa : aas) =
        case aa of
          "--" -> case getAPCData apcData of
                    ArgsParseControl ArgsComplete ArgsHardDash -> 
                      parseError usage ("unexpected -- " ++
                        "(extra arguments not allowed)")
                    _ -> ([], (am, posn, (rest ++ aas)))
          s@('-' : '-' : name) 
            | isJust (Map.lookup name name_hash) ||
              apcDash (getAPCData apcData) == ArgsHardDash ->
              case Map.lookup name name_hash of
                Just ad -> 
                  let (args', am') = peel s ad aas in
                  (args', (am', posn, rest))
                Nothing ->
                  case getAPCData apcData of
                    ArgsParseControl ArgsInterspersed _ ->
                      (aas, (am, posn, rest ++ ["--" ++ name]))
                    _ -> 
                      parseError usage
                        ("unknown argument --" ++ name)
          ('-' : abbr : abbrs)
            | isJust (Map.lookup abbr abbr_hash) ||
              apcDash (getAPCData apcData) == ArgsHardDash ->
              case Map.lookup abbr abbr_hash of
                Just ad ->
                  let (args', am') = peel ['-', abbr] ad aas
                      state' = (am', posn, rest)
                  in case abbrs of
                    [] -> (args', state')
                    ('-' : _) -> parseError usage
                                 ("bad internal '-' in argument " ++ aa)
                    _ -> (['-' : abbrs] ++ args', state')
                Nothing ->
                    case apcComplete $ getAPCData apcData of
                      ArgsInterspersed ->
                          (aas,
                           (am, posn, rest ++ ['-' : abbr : abbrs]))
                      _ -> parseError usage
                           ("unknown argument -" ++ [abbr])
          _ ->
            case posn of
              (p : ps) ->
                let (_, req_posn) = partition arg_optional posn in
                case length av - length req_posn of
                  n_extra | n_extra > 0 || (n_extra == 0 && arg_required p) ->
                    let (args', am') = peel (dataArgName $ fromJust $ 
                                             argData p) p av in
                    (args', (am', ps, rest))
                  0 -> (av, (am, ps, rest))
                  _ -> parseError usage 
                         "missing required positional argument(s)"
              [] -> ([], (am, [], rest ++ av))
        where
          add_entry s m (k, a) =
              case Map.member k m of
                False -> Map.insert k a m
                True -> parseError usage ("duplicate argument " ++ s)
          peel name (Arg { argData = Nothing, argIndex = index }) argl =
              let am' = add_entry name am (index, ArgvalFlag)
              in (argl, am')
          peel name (Arg { argData = Just (DataArg {}) }) [] =
              parseError usage (name ++ " is missing its argument")
          peel name (Arg { argData = 
                                 Just (DataArg { dataArgArgtype = atype }),
                              argIndex = index })
              (a : argl) =
                let v = case atype of
                          ArgtypeString _ -> ArgvalString a
                          ArgtypeInteger _ -> read_arg ArgvalInteger
                                                       "an integer"
                          ArgtypeInt _ -> read_arg ArgvalInt "an int"
                          ArgtypeDouble _ -> read_arg ArgvalDouble "a double"
                          ArgtypeFloat _ -> read_arg ArgvalFloat "a float"
                        where
                          read_arg constructor kind =
                            case reads a of
                              [(val, "")] -> constructor val
                              _ -> parseError usage ("argument " ++
                                                     a ++ " to " ++ name ++
                                                     " is not " ++ kind)
                    am' = add_entry name am (index, v)
                in (argl, am')


-- |Most of the time, you just want the environment's
-- arguments and are willing to live in the IO monad.
-- This version of `parseArgs` digs the pathname and arguments
-- out of the system directly.
parseArgsIO :: (Show a, Ord a, APCData b) =>
               b             -- ^Degree of completeness of parse.
            -> [ Arg a ]     -- ^Argument descriptions.
            -> IO (Args a)   -- ^Argument parse results.
parseArgsIO apcData argd = do
  argv <- getArgs
  pathname <- getProgName
  return (parseArgs apcData argd pathname argv)


-- |Check whether a given optional argument was supplied. Works on all types.
gotArg :: (Ord a) =>
          Args a    -- ^Parsed arguments.
       -> a         -- ^Index of argument to be checked for.
       -> Bool      -- ^True if the arg was present.
gotArg (Args { __args = ArgRecord am }) k =
    case Map.lookup k am of
      Just _ -> True
      Nothing -> False

-- |Type of values that can be parsed by the argument parser.
class ArgType b where

    -- |Fetch an argument's value if it is present.
    getArg :: (Show a, Ord a)
           => Args a    -- ^Parsed arguments.
           -> a         -- ^Index of argument to be retrieved.
           -> Maybe b   -- ^Argument value if present.

    -- |Fetch the value of a required argument.
    getRequiredArg :: (Show a, Ord a)
           => Args a    -- ^Parsed arguments.
           -> a         -- ^Index of argument to be retrieved.
           -> b   -- ^Argument value.

    getRequiredArg ads index =
        case getArg ads index of
          Just v -> v
          Nothing -> error ("internal error: required argument "
                          ++ show index ++ "not supplied")

getArgPrimitive :: Ord a => (Argval -> Maybe b) -> Args a -> a -> Maybe b
getArgPrimitive decons (Args { __args = ArgRecord am }) k =
  Map.lookup k am >>= decons

instance ArgType () where
  getArg = getArgPrimitive (\ArgvalFlag -> return ())

instance ArgType ([] Char) where
  getArg = getArgPrimitive (\(ArgvalString s) -> return s)

-- |[Deprecated]  Return the `String` value, if any, of the given argument.
getArgString :: (Show a, Ord a) =>
                Args a         -- ^Parsed arguments.
             -> a              -- ^Index of argument to be retrieved.
             -> Maybe String   -- ^Argument value if present.
getArgString = getArg

instance ArgType Integer where
  getArg = getArgPrimitive (\(ArgvalInteger i) -> return i)

-- |[Deprecated] Return the `Integer` value, if any, of the given argument.
getArgInteger :: (Show a, Ord a) =>
                 Args a          -- ^Parsed arguments.
              -> a               -- ^Index of argument to be retrieved.
              -> Maybe Integer   -- ^Argument value if present.
getArgInteger = getArg

instance ArgType Int where
  getArg = getArgPrimitive (\(ArgvalInt i) -> return i)

-- |[Deprecated] Return the `Int` value, if any, of the given argument.
getArgInt :: (Show a, Ord a) =>
             Args a      -- ^Parsed arguments.
          -> a           -- ^Index of argument to be retrieved.
          -> Maybe Int   -- ^Argument value if present.
getArgInt = getArg

instance ArgType Double where
  getArg = getArgPrimitive (\(ArgvalDouble i) -> return i)

-- |[Deprecated] Return the `Double` value, if any, of the given argument.
getArgDouble :: (Show a, Ord a) =>
                Args a         -- ^Parsed arguments.
             -> a              -- ^Index of argument to be retrieved.
             -> Maybe Double   -- ^Argument value if present.
getArgDouble = getArg

instance ArgType Float where
  getArg = getArgPrimitive (\(ArgvalFloat i) -> return i)

-- |[Deprecated] Return the `Float` value, if any, of the given argument.
getArgFloat :: (Show a, Ord a) =>
               Args a        -- ^Parsed arguments.
            -> a             -- ^Index of argument to be retrieved.
            -> Maybe Float   -- ^Argument value if present.
getArgFloat = getArg

-- |`ArgType` instance for opening a file from its string name.
newtype ArgFileOpener = ArgFileOpener {
      argFileOpener :: IOMode -> IO Handle  -- ^Function to open the file
    }

instance ArgType ArgFileOpener where
    getArg ads index =
        getArg ads index >>= 
          (\s -> return $ ArgFileOpener { argFileOpener = openFile s })

-- |[Deprecated] Treat the `String` value, if any, of the given argument as
-- a file handle and try to open it as requested.
getArgFile :: (Show a, Ord a) =>
              Args a              -- ^Parsed arguments.
           -> a                   -- ^Index of argument to be retrieved.
           -> IOMode              -- ^IO mode the file should be opened in.
           -> IO (Maybe Handle)   -- ^Handle of opened file, if the argument
                                  -- was present.
getArgFile ads k m =
  case getArg ads k of
    Just fo -> (do h <- argFileOpener fo m; return (Just h))
    Nothing -> return Nothing


-- |Treat the `String` value, if any, of the given argument as a
-- file handle and try to open it as requested.  If not
-- present, substitute the appropriate one of stdin or
-- stdout as indicated by `IOMode`.
getArgStdio :: (Show a, Ord a) =>
               Args a      -- ^Parsed arguments.
            -> a           -- ^Index of argument to be retrieved.
            -> IOMode      -- ^IO mode the file should be opened in.
                           -- Must not be `ReadWriteMode`.
            -> IO Handle   -- ^Appropriate file handle.
getArgStdio ads k m =
    case getArg ads k of
      Just s -> openFile s m
      Nothing ->
          case m of
            ReadMode -> return stdin
            WriteMode -> return stdout
            AppendMode -> return stdout
            ReadWriteMode ->
                     error ("internal error: tried to open stdio "
                            ++ "in ReadWriteMode")

---
--- Misc
---

-- |Return the filename part of a pathname.
-- Unnecessarily efficient implementation does a single
-- tail-call traversal with no construction.
baseName :: String   -- ^Pathname.
         -> String   -- ^Rightmost component of pathname.
baseName s =
    let s' = dropWhile (/= '/') s in
    if null s' then s else baseName (tail s')


-- |Generate a usage error with the given supplementary message string.
usageError :: (Ord a) => Args a -> String -> b
usageError ads msg = error (argsUsage ads ++ "\n" ++ msg)
