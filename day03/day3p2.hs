#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import Data.List (transpose)
import Data.List.Split (chunksOf)
main :: IO ()
main = interact $ show . length . filter id . map (valid . parsein) . parse

parse :: String -> [[Int]]
parse = concat . map transpose . chunksOf 3 . map (map read . words) . lines

parsein :: [Int] -> (Int, Int, Int)
parsein s = let (a:b:c:_) = s
            in (a,b,c)

valid :: (Int, Int, Int) -> Bool
valid (a,b,c) =    a + b > c
                && a + c > b
                && b + c > a
