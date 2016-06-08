-- Stupid glue for "cabal test"
-- http://stackoverflow.com/a/31214045/364875

import System.Exit
import System.Process

main = do
  result <- system "./test-parseargs.sh"
  case result of
    ExitSuccess -> return ()
    _ -> exitFailure
