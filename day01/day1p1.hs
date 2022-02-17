#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import Data.List.Split (splitOn)
data Direction = N | W | S | E
data Turn = L | R
type Instruction = (Turn, Integer)
type Position = (Direction, Integer, Integer)

main :: IO ()
main = interact $ show . dist . foldl takeStep startPos . parsein

takeStep :: Position -> Instruction -> Position
takeStep (d, x0, y0) (L, n) = let d' = turnLeft  d in (d', addx d' x0 n, addy d' y0 n)
takeStep (d, x0, y0) (R, n) = let d' = turnRight d in (d', addx d' x0 n, addy d' y0 n)

addx E x0 n = x0 + n
addx W x0 n = x0 - n
addx _ x0 _ = x0
addy N y0 n = y0 + n
addy S y0 n = y0 - n
addy _ y0 _ = y0

dist :: Position -> Integer
dist (_, x, y) = abs x + abs y

startPos :: Position
startPos = (N, 0, 0)

parsein :: String -> [Instruction]
parsein = map toInstruction . splitOn ", "

toInstruction :: String -> Instruction
toInstruction ('L':as) = (L, read as)
toInstruction ('R':as) = (R, read as)
toInstruction s = error $ "Parse issue: " ++ s

turnRight :: Direction -> Direction
turnRight N = E
turnRight W = N
turnRight S = W
turnRight E = S

turnLeft :: Direction -> Direction
turnLeft N = W
turnLeft W = S
turnLeft S = E
turnLeft E = N
