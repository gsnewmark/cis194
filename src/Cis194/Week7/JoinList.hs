{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
module Cis194.Week7.JoinList where

import Data.Monoid

import Buffer
import Cis194.Week7.Scrabble
import Sized

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)

tag :: Monoid m => JoinList m a -> m
tag (Single m _)   = m
tag (Append m _ _) = m
tag p              = mempty p

jSize :: (Sized b, Monoid b) => JoinList b a -> Int
jSize = getSize . size . tag

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
l +++ r = Append (tag l <> tag r) l r

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ 0 (Single _ x)   = Just x
indexJ i (Append _ l r)
   | i < ls      = indexJ i l
   | i < rs + ls = indexJ (i - ls) r
   where ls = jSize l
         rs = jSize r
indexJ _ _              = Nothing

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ n jl | n <= 0    = jl
dropJ _ (Single _ _)   = Empty
dropJ i (Append _ l r)
   | i < ls       = dropJ i l +++ r
   | i <= rs + ls = dropJ (i - ls) r
   where ls = jSize l
         rs = jSize r
dropJ _ _              = Empty

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ n _ | n <= 0       = Empty
takeJ _ jl@(Single _ _)  = jl
takeJ i (Append _ l r)
   | i <= ls    = takeJ i l
   | otherwise  = l +++ takeJ (i - ls) r
   where ls = jSize l
takeJ _ jl               = jl

scoreLine :: String -> JoinList Score String
scoreLine s = Single (scoreString s) s

jlToList :: JoinList m a -> [a]
jlToList Empty            = []
jlToList (Single _ a)     = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

instance Buffer (JoinList (Score, Size) String) where
  toString     = unlines . jlToList
  fromString   = foldr (+++) Empty . map (\x -> Single (scoreString x, Size 1) x) . lines
  line         = indexJ
  replaceLine n l b = p +++ Single (scoreString l, Size 1) l +++ s
      where p = takeJ n b
            s = dropJ (n + 1) b
  numLines     = jSize
  value        = getScore . xscore . tag

