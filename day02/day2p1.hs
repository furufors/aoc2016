#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
data Direction = U | D | L | R

main :: IO ()
main = interact $ show . solve . map parsein . lines

solve :: [[Direction]] -> [Integer]
solve dss = solve2 5 dss
    where
        solve2 :: Integer -> [[Direction]] -> [Integer]
        solve2 _ [      ] = []
        solve2 a (ds:dss) = let i1 = foldl step a ds
                            in i1:(solve2 i1 dss)

step :: Integer -> Direction -> Integer
step i L | i `elem` [1,4,7] = i
step i R | i `elem` [3,6,9] = i
step i U | i `elem` [1,2,3] = i
step i D | i `elem` [7,8,9] = i
step i R = i + 1
step i L = i - 1
step i D = i + 3
step i U = i - 3

parsein :: String -> [Direction]
parsein = map toDirection

toDirection :: Char -> Direction
toDirection 'U' = U
toDirection 'D' = D
toDirection 'L' = L
toDirection 'R' = R
toDirection  c  = error $ "missing char: " ++ [c]
