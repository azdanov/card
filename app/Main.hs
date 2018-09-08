module Main where

import           Card

main :: IO ()
main = do
  putStrLn "Enter a Card Number:"
  line <- getLine

  if isValid $ read line
    then putStrLn $ "Number" ++ " " ++ line ++ " " ++ "is valid."
    else putStrLn $ "Number" ++ " " ++ line ++ " " ++ "is invalid."
