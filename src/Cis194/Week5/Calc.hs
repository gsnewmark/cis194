{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Cis194.Week5.Calc where

import ExprT
import Parser
import qualified StackVM as VM

eval :: ExprT -> Integer
eval (Lit n) = n
eval (Add l r) = eval l + eval r
eval (Mul l r) = eval l * eval r

evalStr :: String -> Maybe Integer
evalStr = fmap eval . parseExp Lit Add Mul

class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr ExprT where
  lit = Lit
  add = Add
  mul = Mul

reify :: ExprT -> ExprT
reify = id

instance Expr Integer where
  lit = id
  add = (+)
  mul = (*)

instance Expr Bool where
  lit = (>0)
  add = (||)
  mul = (&&)

newtype MinMax = MinMax Integer deriving (Eq, Show, Ord)
newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr MinMax where
  lit = MinMax
  add = max
  mul = min

instance Expr Mod7 where
  lit n = Mod7 (mod n 7)
  add (Mod7 l) (Mod7 r) = lit $ l + r
  mul (Mod7 l) (Mod7 r) = lit $ l * r

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger = testExp :: Maybe Integer
testBool = testExp :: Maybe Bool
testMM = testExp :: Maybe MinMax
testSat = testExp :: Maybe Mod7

instance Expr VM.Program where
  lit = flip (:) []. VM.PushI
  add l r = l ++ r ++ [VM.Add]
  mul l r = l ++ r ++ [VM.Mul]

compile :: String -> Maybe VM.Program
compile = parseExp lit add mul
