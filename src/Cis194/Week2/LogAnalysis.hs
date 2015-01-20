module Cis194.Week2.LogAnalysis where

import Log

parseMessage :: String -> LogMessage
parseMessage msg = case words msg of
  ("I":ts:m)       -> LogMessage Info (read ts) (unwords m)
  ("W":ts:m)       -> LogMessage Warning (read ts) (unwords m)
  ("E":level:ts:m) -> LogMessage (Error (read level)) (read ts) (unwords m)
  _                -> Unknown msg

parse :: String -> [LogMessage]
parse = map parseMessage . lines

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tr = tr
insert m Leaf = Node Leaf m Leaf
insert m@(LogMessage _ ts _) (Node l nm@(LogMessage _ tts _) r)
  | ts <= tts = Node (insert m l) nm r
  | otherwise = Node l nm (insert m r)

build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf         = []
inOrder (Node l n r) = inOrder l ++ [n] ++ inOrder r

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = map msg . filter severeError . inOrder . build
  where msg (LogMessage _ _ m) = m
        msg (Unknown m)        = m
        severeError (LogMessage (Error lvl) _ _) = lvl >= 50
        severeError _                            = False

