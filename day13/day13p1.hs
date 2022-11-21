#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

import Algorithm.Search
import Data.Maybe
type State = (Int,Int)

main :: IO ()
main = interact $ show . fromJust . solve . read

solve :: Int -> Maybe (Int, [State])
solve key = dijkstra next cost solved start
    where
        solved :: State -> Bool
        solved = (==(31,39))
        start :: State
        start = (1,1)
        next :: State -> [State]
        next (x,y) = filter (\p -> not (blocked p) && valid p) [(x,y+1), (x+1,y), (x,y-1), (x-1,y)]
        blocked :: State -> Bool
        blocked (x,y) = let num = x*x + 3*x + 2*x*y + y + y*y + key
                        in oddSetBits num
        cost :: State -> State -> Int
        cost _ _ = 1
        valid :: State -> Bool
        valid (x,y) = x >= 0 && y >= 0
        oddSetBits :: Int -> Bool
        oddSetBits = odd . sum . toBits
        toBits :: Int -> [Int]
        toBits 0 = [0]
        toBits n | n `mod` 2 == 1 = toBits (n `div` 2) ++ [1]
                 | n `mod` 2 == 0 = toBits (n `div` 2) ++ [0]