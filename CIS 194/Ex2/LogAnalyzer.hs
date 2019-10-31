{-# OPTIONS_GHC -Wall  #-}

module LogAnalyzer where

import Log 



parseMessage :: String -> LogMessage
parseMessage c 
        | first == "E" = LogMessage (Error (read second::Int)) (read third::Int) fromFourth
        | first == "I" = LogMessage Info (read second::Int) fromThird
        | otherwise = undefined
        where arr = words c
              first = head $ arr
              second = head $ tail arr
              third = head $ tail $ tail arr
              fromThird = unwords $  drop 2 arr
              fromFourth = unwords $ drop 3 arr

insert :: LogMessage  -> MessageTree -> MessageTree
insert lm Leaf = Node Leaf lm Leaf
insert (Unknown _) mt= mt 
insert l1@(LogMessage _ x _) m@(Node left l2@(LogMessage _ y _) right) 
        | x <= y = Node (insert l1 left) l2 right
        | x > y = Node left l2 (insert l1 right) 
        | otherwise = m
insert _ m@(Node _ (Unknown _) _) = m


build ::[LogMessage] -> MessageTree
build = undefined

mTree :: MessageTree
mTree = Node Leaf (parseMessage "I 29 la la la") Leaf