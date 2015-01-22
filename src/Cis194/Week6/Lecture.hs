module Cis194.Week6.Lecture where

(&&) :: Bool -> Bool -> Bool
True  && x = x
False && _ = False

(&&!) :: Bool -> Bool -> Bool
True  &&! True  = True
True  &&! False = False
False &&! True  = False
False &&! False = False

if' :: Bool -> a -> a -> a
if' True  x _ = x
if' False _ y = y
