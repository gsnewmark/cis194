{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Cis194.Week12.Risk where

import Control.Monad
import Control.Monad.Random
import Data.List

------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int }
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random           = first DV . randomR (1,6)
  randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army }

battle :: Battlefield -> Rand StdGen Battlefield
battle b = replicateM (a + d) die >>= \ds ->
  return $ r $ c $ dr ds
  where (a,d) = (min 3 (attackers b - 1), min 2 $ defenders b)
        dr ds = (\[x,y] -> (x,y)) $ fmap (reverse . sort) $ (\(x,y) -> [x,y]) $ splitAt a ds
        c (ad,dd) = (length t - w, w)
          where t = zipWith (>) ad dd
                w = length $ filter id t
        r (ac,dc) = Battlefield (attackers b - ac) (defenders b - dc)

invade :: Battlefield -> Rand StdGen Battlefield
invade b = battle b >>= f
  where f bf@(Battlefield a d) | d == 0 || a < 2 = return bf
                               | otherwise       = invade bf

successProb :: Battlefield -> Rand StdGen Double
successProb b = replicateM 1000 (invade b) >>= \bfs ->
  return $ fl (filter isSuccess bfs) / fl bfs
  where isSuccess (Battlefield a _) = a > 1
        fl = fromIntegral . length

main:: IO()
main = do
  b <- evalRandIO (successProb (Battlefield 10 10))
  print b
