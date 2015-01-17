module Cis194.Week1.Assignment where

toDigits :: Integer -> [Integer]
-- ^Convert positive Integers to a list of digits
toDigits n = reverse (toDigitsRev n)

toDigitsRev :: Integer -> [Integer]
-- ^Convert positive Integers to a list of digits with digits reversed
toDigitsRev n
  | n <= 0    = []
  | otherwise = mod n 10 : toDigitsRev (div n 10)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther [x] = [x]
doubleEveryOther (x:y:xs)
  | mod (length xs) 2 == 1 = x : (y * 2) : doubleEveryOther xs
  | otherwise              = (x * 2) : y : doubleEveryOther xs

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs)
  | div x 10 /= 0 = div x 10 + mod x 10 + sumDigits xs
  | otherwise     = x + sumDigits xs

validate :: Integer -> Bool
validate n = mod (sumDigits (doubleEveryOther (toDigits n))) 10 == 0

type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n s d t
  | n <= 0    = []
  | otherwise = hanoi (n - 1) s t d ++ [(s, d)] ++ hanoi (n - 1) t d s
