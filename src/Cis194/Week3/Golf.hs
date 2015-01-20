module Cis194.Week3.Golf where

import Data.List

skips :: [a] -> [[a]]
skips xs = map t (take (length xs) (zip (repeat xs) [1..]))
  where t :: ([a], Integer) -> [a]
        t (l,n) = map snd (filter f (zip [1..] l))
          where f (i, _) = mod i n == 0

localMaxima :: [Integer] -> [Integer]
localMaxima = map x . filter f . tails
  where f (a:b:c:_) = b > a && b > c
        f _ = False
        x (_:b:_:_) = b

padR :: Int -> String -> String
padR n s
  | length s < n = s ++ replicate (n - length s) ' '
  | otherwise = s

histogram :: [Integer] -> String
histogram a = (intercalate "\n" . reverse . transpose . map f) [0..9]
  where f x = padR m (show x ++ "=" ++ replicate (l x) '*')
        l x = maybe 0 length (lookup x c)
        m = maximum (map (length . snd) c) + 2
        c = (map toPair . group . sort) a
          where toPair p@(x:_) = (x,p)
