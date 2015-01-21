module Cis194.Week4.Assignment where

import Data.List (genericLength)

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
  | even x    = (x - 2) * fun1 xs
  | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = product . map (subtract 2) . filter even

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n | even n    = n + fun2 (n `div` 2)
       | otherwise = fun2 (3 * n + 1)

fun2' :: Integer -> Integer
fun2' = sum . filter even . takeWhile (>1) . iterate f
  where f i | even i    = div i 2
            | otherwise = 3 * i + 1

data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree = foldr insert Leaf
  where insert e Leaf = Node 0 Leaf e Leaf
        insert e (Node h l@Leaf x r) = Node (h + 1) (insert e l) x r
        insert e (Node h l x r@Leaf) = Node h l x (insert e r)
        insert e (Node h l@(Node lh _ _ _) x r@(Node rh _ _ _))
          | lh <= rh  = let l' = insert e l
                            (Node lh' _ _ _) = l' in
                         Node (h + (lh' - lh)) l' x r
          | otherwise = Node h l x (insert e r)

xor :: [Bool] -> Bool
xor = foldr (\_ acc -> not acc) False . filter (==True)

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x xs -> f x : xs) []

sieveSundaram :: Integer -> [Integer]
sieveSundaram = map (succ . (*2)) .
                (\xs -> filter (\x -> notElem x (s (genericLength xs))) xs) .
                reverse . takeWhile (>0) . iterate (subtract 1)
  where s n = [x | j <- [1..n], i <- [1..j], let x = i + j + 2 * i *j, x <= n]
