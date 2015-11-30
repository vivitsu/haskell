{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import Log

parseMessage :: String -> LogMessage
parseMessage s = go $ words s
    where go ("I":x:xs)   = LogMessage Info (read x) (unwords xs)
          go ("W":x:xs)   = LogMessage Warning (read x) (unwords xs)
          go ("E":x:y:xs) = LogMessage (Error (read x)) (read y) (unwords xs)
          go l            = Unknown (unwords l)

parse :: String -> [LogMessage]
parse s = parseMessage <$> lines s

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert msg Leaf = Node Leaf msg Leaf
insert msg (Node left node right)
    | msg < node = Node (insert msg left) node right
    | otherwise = Node left node (insert msg right)

build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node l m r) = inOrder l ++ [m] ++ inOrder r

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = helper . filter go . inOrder . build
    where go (LogMessage (Error i) _ _) = i >= 50
          go _ = False
          helper = foldr go2 []
            where go2 (LogMessage _ _ str) acc = str:acc
                  go2 _ acc = acc
