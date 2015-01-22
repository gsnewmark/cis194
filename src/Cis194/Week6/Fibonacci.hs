{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

module Cis194.Week6.Fibonacci where

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

fibs2 :: [Integer]
fibs2 = (:) 0 $ map snd $ iterate f (0, 1)
  where f (pp, p) = (p, pp + p)

data Stream a = a :> (Stream a)

streamToList :: Stream a -> [a]
streamToList (x :> xs) = x : streamToList xs

instance Show a => Show (Stream a) where
  show = flip (++) "..." . (++) "Stream: " . show . take 20 . streamToList

streamRepeat :: a -> Stream a
streamRepeat x = x :> streamRepeat x

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (x :> xs) = f x :> streamMap f xs

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = x :> streamFromSeed f (f x)

nats :: Stream Integer
nats = streamFromSeed succ 0

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (x :> xs) ys = x :> interleaveStreams ys xs

ruler :: Stream Integer
ruler = foldr1 interleaveStreams (map streamRepeat [0..])

x :: Stream Integer
x = 0 :> (1 :> streamRepeat 0)

instance Num (Stream Integer) where
  fromInteger n = n :> streamRepeat 0
  negate = streamMap negate
  (x :> xs) + (y :> ys) = (x + y) :> (xs + ys)
  (x :> xs) * ysi@(y :> ys) = (x * y) :> ((streamMap (*x) ys) + (xs * ysi))

instance Fractional (Stream Integer) where
  (x :> xs) / (y :> ys) = q
    where q = (div x y) :> (xs - (streamMap (flip div y) (q * ys)))

fibs3 :: Stream Integer
fibs3 = x / (1 - x - x * x)
