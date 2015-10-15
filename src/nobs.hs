module Main where

main = interact (go []) 

go l [] =  reverse l
go l ('\b':c:cs)
    | c == '\b' = go (drop 2 l) cs
    | otherwise = reverse (drop 1 l) ++ go [] (c:cs)
go l ('\b':cs) = go (drop 1 l) cs
go l (c:cs)    = go (c:l)cs
