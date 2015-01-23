{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Buffer
import Cis194.Week7.JoinList
import Cis194.Week7.Scrabble
import Editor
import Sized

main = let b :: JoinList (Score, Size) String = fromString $ unlines [ "This buffer is for notes you don't want to save, and for", "evaluation of steam valve coefficients.", "To load a different file, type the character L followed", "by the name of the file."] in
        runEditor editor b
