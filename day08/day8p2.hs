{-# Language LambdaCase #-}
#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import Data.List (isPrefixOf)

data OnOff = On | Off
data Action  = Rect Int Int | Row Int Int | Col Int Int
type Cell = [[OnOff]]

main :: IO ()
main = interact $ draw . foldl step emptyCell . map parsein . lines

step :: Cell -> Action -> Cell
step c (Rect x y) = [[ if ir < y && ic < x then On else c!!ir!!ic | ic <- [0..49] ] | ir <- [0..5] ]
step c (Col  i o) = [[ if ic == i then c!!((ir - o) `mod`  6)!!ic else c!!ir!!ic | ic <- [0..49] ] | ir <- [0..5] ]
step c (Row  i o) = [[ if ir == i then c!!ir!!((ic - o) `mod` 50) else c!!ir!!ic | ic <- [0..49] ] | ir <- [0..5] ]

emptyCell :: Cell
emptyCell = replicate 6 $ replicate 50 Off

toChar :: OnOff -> Char
toChar On  = '#'
toChar Off = '.'

draw :: Cell -> String
draw = unlines . map (map toChar)

parsein :: String -> Action
parsein s | isPrefixOf "rect" s = let a = drop 5 s
                                      x = read $ takeWhile (`elem` ['0'..'9']) a
                                      y = read . drop 1 $ dropWhile (`elem` ['0'..'9']) a
                                  in Rect x y
parsein s | isPrefixOf "rotate row" s = let a = drop 13 s
                                            i = read $ takeWhile (`elem` ['0'..'9']) a
                                            o = read . drop 4 $ dropWhile (`elem` ['0'..'9']) a
                                        in Row i o
parsein s | isPrefixOf "rotate column" s = let a = drop 16 s
                                               i = read $ takeWhile (`elem` ['0'..'9']) a
                                               o = read . drop 4 $ dropWhile (`elem` ['0'..'9']) a
                                           in Col i o
parsein s = error $ "Cannot parse string: " ++ s
