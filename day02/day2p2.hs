#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
data Direction = U | D | L | R

main :: IO ()
main = interact $ show . solve 5 . map parsein . lines

solve :: Integer -> [[Direction]] -> [Integer]
solve _ [      ] = []
solve a (ds:dss) = let i1 = foldl step a ds
                   in i1:(solve i1 dss)

step :: Integer -> Direction -> Integer
step i L | i `elem` [1, 2, 5,10,13] = i
step i R | i `elem` [1, 4, 9,12,13] = i
step i U | i `elem` [5, 2, 1, 4, 9] = i
step i D | i `elem` [5,10,13,12, 9] = i
step i R = i + 1
step i L = i - 1
step 3  U = 1
step 1  D = 3
step 13 U = 11
step 11 D = 13
step i D = i + 4
step i U = i - 4

parsein :: String -> [Direction]
parsein = map toDirection

toDirection :: Char -> Direction
toDirection 'U' = U
toDirection 'D' = D
toDirection 'L' = L
toDirection 'R' = R
toDirection  c  = error $ "missing char: " ++ [c]

--     1
--   2 3 4
-- 5 6 7 8 9
--   A B C
--     D
