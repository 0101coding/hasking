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
              fromThird = unwords $  drop 4 arr
              fromFourth = unwords $ drop 4 arr


data MessageTree = Leaf | Node MessageTree LogMessage MessageTree

insert :: LogMessage  -> MessageTree -> MessageTree

insert x (Node left v right) |
            x < v = Node (insert x left) v right
            x > v = Node right v (insert x right) 
