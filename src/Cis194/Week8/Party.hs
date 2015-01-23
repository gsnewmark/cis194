{-# OPTIONS_GHC -fno-warn-orphans #-}
module Cis194.Week8.Party where

import Data.List
import Data.Monoid
import Data.Tree

import Employee

glCons :: Employee -> GuestList -> GuestList
glCons e (GL es f) = GL (e : es) (f + empFun e)

instance Monoid GuestList where
  mempty                        = GL [] 0
  mappend (GL el fl) (GL er fr) = GL (el ++ er) (fl + fr)

moreFun :: GuestList -> GuestList -> GuestList
moreFun = max

treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold f (Node x t) = f x $ map (treeFold f) t

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel e [] = (GL [e] (empFun e), GL [] 0)
nextLevel e ts = (wb, nb)
  where wb = glCons e $ findBest $ map (\(wd, nd) -> (withoutFF wd, nd)) ts
        nb = findBest ts
        withoutFF (GL (e:es) f) = GL (e:es) (f - empFun e)
        findBest = mconcat . map (uncurry moreFun)

maxFun :: Tree Employee -> GuestList
maxFun = uncurry max . treeFold nextLevel

showSortedGuestList :: GuestList -> String
showSortedGuestList (GL es f) =
  unlines $ ("Total fun: " ++ show f) : sort (map empName es)

main :: IO ()
main = do
  contents <- readFile "company.txt"
  putStr $ showSortedGuestList $ maxFun $ read contents
