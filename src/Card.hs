module Card where

--------------------------------------
-- Validating Credit Card Numbers
--------------------------------------

import           Data.Char

-- ===================================

toDigits :: Integer -> [Integer]
toDigits x | x < 0     = error "Negative number provided"
           | otherwise = map (toInteger . digitToInt) $ show x

-- ===================================

toDigitsRev :: Integer -> [Integer]
toDigitsRev x | x < 0     = error "Negative number provided"
              | otherwise = reverse $ toDigits x

-- ===================================

doubleSecond :: [Integer] -> [Integer]
doubleSecond xs = zipWith (curry f) xs [0 ..]
 where
  f y | even $ snd y = fst y
      | otherwise    = fst y * 2

-- ===================================

sumDigits :: [Integer] -> Integer
sumDigits = sum . map f
 where
  f x | x < 0     = error "Negative number provided"
      | x < 9     = x
      | otherwise = sum $ toDigits x

-- ===================================

isValid :: Integer -> Bool
isValid = f . sumDigits . doubleSecond . toDigitsRev
  where f x = x `mod` 10 == 0

-- ===================================

numValid :: [Integer] -> Integer
numValid = sum . map (const 1) . filter isValid

