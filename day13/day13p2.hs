#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

import Algorithm.Search
import Data.Maybe
type State = (Int,Int)

main :: IO ()
main = interact $ show . search . read

search :: Int -> Int
search key = sum . map toScore $ [(x,y) | x <- [0..51], y <- [0..51], (x-1)*(y-1) <= 50*50]
    where
        toScore :: State -> Int
        toScore target = let res = dijkstra next (\_ -> \_ -> 1) (==target) (1,1)
                         in case res of
                                Just (l,_) -> if l <= 50 then 1 else 0
                                Nothing -> 0
        next :: State -> [State]
        next (x,y) = filter (\p -> not (blocked p) && valid p) [(x,y+1), (x+1,y), (x,y-1), (x-1,y)]
        blocked :: State -> Bool
        blocked (x,y) = let num = x*x + 3*x + 2*x*y + y + y*y + key
                        in oddSetBits num
        valid :: State -> Bool
        valid (x,y) = x >= 0 && y >= 0
        oddSetBits :: Int -> Bool
        oddSetBits = odd . sum . toBits
        toBits :: Int -> [Int]
        toBits 0 = [0]
        toBits n | n `mod` 2 == 1 = toBits (n `div` 2) ++ [1]
                 | n `mod` 2 == 0 = toBits (n `div` 2) ++ [0]