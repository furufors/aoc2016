#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import Data.List
import Data.Ord

main :: IO ()
main = interact $ show . numberOfAllowed 4294967296 . converge (reverse . combineAll . reverse . combineAll . nub . sortBy (comparing fst)) . map parse . lines

overlapOrAdjacent :: (Int,Int) -> (Int, Int) -> Bool
overlapOrAdjacent (a,b) (c,d) = (a >= c && a <= d) || (b >= c && b <= d) || (c >= a && c <= b) || (d >= a && d <= b) ||  abs (b - c) == 1 || abs (a - d) == 1

combine :: (Int,Int) -> (Int, Int) -> (Int,Int)
combine (a,b) (c,d) = (min a c, max b d)

combineAll :: [] (Int,Int) -> [] (Int,Int)
combineAll [] = []
combineAll (r:rs) = r:(combineAll $ map (\r' -> if overlapOrAdjacent r r' then combine r r' else r') rs)

converge :: Eq a => (a -> a) -> a -> a
converge f a = let a' = f a in if a == a' then a else converge f a'

parse :: String -> (Int,Int)
parse s = let (a,b) = span (/='-') s in (read a, read (tail b))

numberOfAllowed :: Int -> [] (Int,Int) -> Int
numberOfAllowed i [] = i
numberOfAllowed i ((l,h):ps) = numberOfAllowed (i - (h-l+1)) ps