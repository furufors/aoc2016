#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import Data.List (transpose)
import Data.List.Split (chunksOf)
main :: IO ()
main = interact $ show . length . filter id . map valid . concat . map transpose . chunksOf 3 . map (map read . words) . lines

valid :: [Int] -> Bool
valid (a:b:c:[]) =    a + b > c
                   && a + c > b
                   && b + c > a
valid _ = error "A row contains more or less than three elements."
