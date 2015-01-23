{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Cis194.Week7.Scrabble where

import Data.Char (toLower)
import Data.Monoid

newtype Score = Score Int
  deriving (Eq, Ord, Show, Num)

instance Monoid Score where
  mempty  = Score 0
  mappend = (+)

getScore :: Score -> Int
getScore (Score i) = i

class Scorable a where
  xscore :: a -> Score

instance Scorable Score where
  xscore = id

instance Scorable a => Scorable (a,b) where
  xscore = xscore . fst

score :: Char -> Score
score c
  | cl `elem` "eaionrtlsu" = Score 1
  | cl `elem` "dg"         = Score 2
  | cl `elem` "bcmp"       = Score 3
  | cl `elem` "fhvwy"      = Score 4
  | cl == 'k'              = Score 5
  | cl `elem` "jx"         = Score 8
  | cl `elem` "qz"         = Score 10
  | otherwise              = Score 0
  where cl = toLower c

scoreString :: String -> Score
scoreString = mconcat . map score
