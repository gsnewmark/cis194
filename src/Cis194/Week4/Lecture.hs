module Cis194.Week4.Lecture where

gt100 :: Integer -> Bool
gt100 x = x > 100

greaterThan100 :: [Integer] -> [Integer]
greaterThan100 xs = filter gt100 xs

greaterThan100_2 :: [Integer] -> [Integer]
greaterThan100_2 xs = filter (\x -> x > 100) xs

greaterThan100_3 :: [Integer] -> [Integer]
greaterThan100_3 xs = filter (>100) xs

foo :: (b -> c) -> (a -> b) -> (a -> c)
foo f g = \x -> f (g x)

myTest :: [Integer] -> Bool
myTest xs = even (length (greaterThan100 xs))

myTest' :: [Integer] -> Bool
myTest' = even . length . greaterThan100

sum' :: [Integer] -> Integer
sum' []     = 0
sum' (x:xs) = x + sum' xs

product' :: [Integer] -> Integer
product' [] = 1
product' (x:xs) = x * product' xs

length' :: [a] -> Int
length' []     = 0
length' (_:xs) = 1 + length' xs

fold :: b -> (a -> b -> b) -> [a] -> b
fold z f []     = z
fold z f (x:xs) = f x (fold z f xs)

sum''     = fold 0 (+)
product'' = fold 1 (*)
length''  = fold 0 (\_ s -> 1 + s)
