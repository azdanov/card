module Main where

import           System.Environment (getArgs)
import           System.Exit (exitFailure)
import           Text.Read (readMaybe)

import           Card (isValid)


-- A utility function that prints a message to stdout and terminates the
-- programme.
die :: String -> IO a
die s = do
  putStrLn s
  exitFailure


main :: IO ()
main = do
  args <- getArgs
  input <- case args of
    [] -> getInteractiveInput
    ["-"] -> getStdinInput
    _ -> die "Invalid command line"

  putStrLn $ "Number " ++ show input ++ " is " ++ printValid (isValid input)
  where
    printValid :: Bool -> String
    printValid True = "valid"
    printValid False = "not valid"


-- `read` throws a (not very informative) error if the input is not a number. We
-- can use `readMaybe` to handle this case, printing a slightly more informative
-- error message.
ensureInteger :: String -> IO Integer
ensureInteger s = case readMaybe s of
  Just n -> return n
  Nothing -> die $ "Not a number: " ++ s


getInteractiveInput :: IO Integer
getInteractiveInput = do
  putStrLn "Enter a Card Number:"
  ensureInteger =<< getLine
  -- `=<<` has type `Monad m => (a -> m b) -> m a -> m b`. `IO` has a monad
  -- instance which lets `=<<` pass the output of the computation `getLine`, a
  -- string, as input to the computation `ensureInteger`.


getStdinInput :: IO Integer
getStdinInput = ensureInteger =<< getContents
  -- `getContents` returns all input from stdin.
