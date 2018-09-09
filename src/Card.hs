{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- This is required for the `deriving (Num, Real, Integral)` of `Digit`. Since a
-- newtype contains the same data as the type it is wrapping (here `Int`), we
-- can derive any class for which the wrapped type has an instance. This
-- deriving strategy is not part of standard Haskell, so we must activate the
-- `GeneralizedNewtypeDeriving` extension to use it.

module Card where

--------------------------------------
-- Validating Credit Card Numbers
--------------------------------------

import           Data.Char (digitToInt)
import           Data.Maybe (fromJust)

-- Invariant: A digit is in the range 0..9.
newtype Digit = Digit { fromDigit :: Int }
  deriving (Eq, Ord, Read, Show, Enum, Num, Real, Integral)

toDigits :: Integer -> Maybe [Digit]
toDigits x | x < 0     = Nothing
           | otherwise = Just $ map (Digit . digitToInt) $ show x

doubleSecond :: [Digit] -> [Integer]
doubleSecond xs = zipWith f xs [0 ..]
 where
  f :: Digit -> Int -> Integer
  f x i
    | even i    = toInteger x
    | otherwise = toInteger x * 2

isValid :: Integer -> Bool
isValid n = case toDigits n of
  Nothing -> False
  Just digits ->
    f $ sum $ concatMap (fromJust . toDigits) $ doubleSecond $ reverse digits
  where f x = x `mod` 10 == 0

numValid :: [Integer] -> Integer
numValid = toInteger . length . filter isValid
