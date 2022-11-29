#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

data Plate = Safe | Trap deriving (Show, Eq)
main :: IO ()
main = interact $ show . calculate 400000 0 . map parse . head . lines

calculate :: Int -> Int -> [Plate] -> Int
calculate 0 n _  = n
calculate i n ps = calculate (i-1) (n + (sum . map toInt) ps) (newRow ps)

addRow :: [[Plate]] -> [[Plate]]
addRow ps = ps ++ [newRow $ last ps]

newRow :: [Plate] -> [Plate]
newRow p = checkTile $ [Safe] ++ p ++ [Safe]

checkTile :: [Plate] -> [Plate]
checkTile (a:b:c:ds) =
    let rest = checkTile (b:c:ds)
    in case (a,b,c) of
        (Trap,Trap,Safe) -> Trap:rest
        (Safe,Trap,Trap) -> Trap:rest
        (Safe,Safe,Trap) -> Trap:rest
        (Trap,Safe,Safe) -> Trap:rest
        otherwise        -> Safe:rest
checkTile _ = []

toInt :: Plate -> Int
toInt Safe = 1
toInt Trap = 0

parse :: Char -> Plate
parse '.' = Safe
parse '^' = Trap
parse x   = error $ "Plate descrition " ++ [x] ++ " is not valid"