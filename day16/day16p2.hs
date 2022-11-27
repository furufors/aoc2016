#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import Data.List.Split (chunksOf)

main :: IO ()
main = interact $ concat . map show . checksum . take discSize . filldisk . parse . head . lines

checksum :: [Int] -> [Int]
checksum is = let cs = calcChecksum is
              in if even $ length cs
                 then checksum cs
                 else cs

calcChecksum :: [Int] -> [Int]
calcChecksum is = let chunks = chunksOf 2 is
                  in map same chunks

same :: [Int] -> Int
same (a:b:_) = if a==b then 1 else 0
same _ = error "Same recevied array of length < 2"

filldisk :: [Int] -> [Int]
filldisk is | length is >= discSize = is
filldisk is = filldisk $ is ++ [0] ++ (map change . reverse $ is)

change :: Int -> Int
change 0 = 1
change 1 = 0
change x = error "Non 0/1 value in change"

parse :: String -> [Int]
parse = map read . chunksOf 1

discSize :: Int
discSize = 35651584