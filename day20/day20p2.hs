#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import Data.List
import Data.Ord

main :: IO ()
main = interact $ show . numberOfAllowed 4294967296 . converge (unify . nub . combineAll . sortBy (comparing fst)) . map parse . lines
--
overlapOrAdjacent :: (Int,Int) -> (Int, Int) -> Bool
overlapOrAdjacent (a,b) (c,d) = (a >= c && a <= d) || (b >= c && b <= d) || (c >= a && c <= b) || (d >= a && d <= b) ||  b + 1 == c

combine :: (Int,Int) -> (Int, Int) -> (Int,Int)
combine (a,b) (c,d) = (min a c, max b d)

combineAll :: [] (Int,Int) -> [] (Int,Int)
combineAll [] = []
combineAll (r:rs) = r:(combineAll $ map (\r' -> if overlapOrAdjacent r r' then combine r r' else r') rs)

converge :: Eq a => (a -> a) -> a -> a
converge f a = let a' = f a in if a == a' then a else converge f a'

unify :: [] (Int,Int) -> [] (Int,Int)
unify ((l1,h1):(l2,h2):r) = if l2 + 1 <= h1 && h2 >= h1 then unify ((l1,h2):r) else (l1,h1):unify ((l2,h2):r)
unify x = x

parse :: String -> (Int,Int)
parse s = let (a,b) = span (/='-') s in (read a, read (tail b))

numberOfAllowed :: Int -> [] (Int,Int) -> Int
numberOfAllowed i [] = i
numberOfAllowed i ((l,h):ps) = numberOfAllowed (i - (h-l+1)) ps