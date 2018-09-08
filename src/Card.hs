module Card where

--------------------------------------
-- Validating Credit Card Numbers
--------------------------------------

import           Data.Char

toDigits :: Integer -> Maybe [Integer]
toDigits x | x < 0     = Nothing
           | otherwise = Just $ map (toInteger . digitToInt) $ show x

doubleSecond :: [Integer] -> [Integer]
doubleSecond xs = zipWith f xs [0 ..]
 where
  f x i | even i    = x
        | otherwise = x * 2

isValid :: Integer -> Bool
isValid n = case toDigits n of
  Nothing -> False
  Just digits -> f $ sum $ doubleSecond $ reverse digits
  where f x = x `mod` 10 == 0
  -- Here we consume the `Maybe [Integer]` produced by `toDigits`. To do so, we
  -- must distinguish between two cases: Either `toDigits` failed, producing
  -- `Nothing`, or it succeeded, producing `Just` a list of digits. Since
  -- failure means that the number is invalid, we return `False` in that case.

numValid :: [Integer] -> Integer
numValid = toInteger . length . filter isValid
