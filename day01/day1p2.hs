#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import Data.List.Split (splitOn)
data Direction = N | W | S | E
data Turn = L | R
type Instruction = (Turn, Integer)
type Position = (Direction, Integer, Integer)

main :: IO ()
main = interact $ show . dist . firstDouble [] . reverse . foldl takeStep startPos . parsein

takeStep :: [Position] -> Instruction -> [Position]
takeStep (p@(d, x0, y0):ps) (L, n) = let d' = turnLeft  d in (reverse [(d', addx d' x0 s, addy d' y0 s) | s <- [1..n]]) ++ p:ps
takeStep (p@(d, x0, y0):ps) (R, n) = let d' = turnRight d in (reverse [(d', addx d' x0 s, addy d' y0 s) | s <- [1..n]]) ++ p:ps

addx E x0 n = x0 + n
addx W x0 n = x0 - n
addx _ x0 _ = x0
addy N y0 n = y0 + n
addy S y0 n = y0 - n
addy _ y0 _ = y0

firstDouble :: [(Integer, Integer)] -> [Position] -> (Integer, Integer)
firstDouble vis [] = error "No double-visited position found"
firstDouble vis ((_,x,y):ps) =
    let p = (x,y)
    in if p `elem` vis
       then p
       else firstDouble (p:vis) ps

dist :: (Integer, Integer) -> Integer
dist (x, y) = abs x + abs y

startPos :: [Position]
startPos = [(N, 0, 0)]

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
