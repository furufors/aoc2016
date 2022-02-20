#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import Data.List (group, transpose, sort, sortBy)

main :: IO ()
main = interact $ map (head . last . sortBy (\a b -> length a `compare` length b) . group . sort) . transpose . lines
