module Cis194.Week3.Lecture where

data IntList = Empty | Cons Int IntList
  deriving Show

absAll :: IntList -> IntList
absAll Empty       = Empty
absAll (Cons x xs) = Cons (abs x) (absAll xs)

squareAll :: IntList -> IntList
squareAll Empty       = Empty
squareAll (Cons x xs) = Cons (x*x) (squareAll xs)

mapIntList :: (Int -> Int) -> IntList -> IntList
mapIntList _ Empty       = Empty
mapIntList f (Cons x xs) = Cons (f x) (mapIntList f xs)

exampleList = Cons (-1) (Cons 2 (Cons (-6) Empty))

addOne x = x + 1
square x = x * x

-- mapIntList addOne exampleList
-- mapIntList abs    exampleList
-- mapIntList square exampleList

keepOnlyEven :: IntList -> IntList
keepOnlyEven Empty = Empty
keepOnlyEven (Cons x xs)
  | even x    = Cons x (keepOnlyEven xs)
  | otherwise = keepOnlyEven xs

filterIntList :: (Int -> Bool) -> IntList -> IntList
filterIntList _ Empty = Empty
filterIntList p (Cons x xs)
  | p x       = Cons x (filterIntList p xs)
  | otherwise = filterIntList p xs

data List t = E | C t (List t)

lst1 :: List Int
lst1 = C 3 (C 5 (C 2 E))

lst2 :: List Char
lst2 = C 'x' (C 'y' (C 'z' E))

lst3 :: List Bool
lst3 = C True (C False E)

filterList :: (a -> Bool) -> List a -> List a
filterList _ E = E
filterList p (C x xs)
  | p x       = C x (filterList p xs)
  | otherwise = filterList p xs

mapList :: (a -> b) -> List a -> List b
mapList _ E        = E
mapList f (C x xs) = C (f x) (mapList f xs)
