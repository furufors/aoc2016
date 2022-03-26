{-# Language LambdaCase #-}
#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import Data.List (isPrefixOf)
import Text.Parsec hiding (count)

data OnOff = On | Off
data Action  = Rect Int Int | Row Int Int | Col Int Int
type Cell = [[OnOff]]

main :: IO ()
main = interact $ show . count . foldl step emptyCell . map parsein . lines

step :: Cell -> Action -> Cell
step c (Rect x y) = [[ if ir < y && ic < x then On else c!!ir!!ic | ic <- [0..49] ] | ir <- [0..5] ]
step c (Col  i o) = [[ if ic == i then c!!((ir - o) `mod`  6)!!ic else c!!ir!!ic | ic <- [0..49] ] | ir <- [0..5] ]
step c (Row  i o) = [[ if ir == i then c!!ir!!((ic - o) `mod` 50) else c!!ir!!ic | ic <- [0..49] ] | ir <- [0..5] ]

emptyCell :: Cell
emptyCell = replicate 6 $ replicate 50 Off

count :: Cell -> Int
count = sum . map (sum . map toInt)

toInt :: OnOff -> Int
toInt On  = 1
toInt Off = 0

parsein :: String -> Action
parsein input = case parse (try parseRect <|> try parseRow <|> try parseColumn) "parsein" input of
    Left err -> error $ show err
    Right a -> a

parseRect :: Parsec String () Action
parseRect = do
    _ <- string "rect "
    x <- read <$> many1 digit
    _ <- string "x"
    y <- read <$> many1 digit
    return $ Rect x y

parseRow :: Parsec String () Action
parseRow = do
    _ <- string "rotate row y="
    i <- read <$> many1 digit
    _ <- string " by "
    o <- read <$> many1 digit
    return $ Row i o

parseColumn :: Parsec String () Action
parseColumn = do
    _ <- string "rotate column x="
    i <- read <$> many1 digit
    _ <- string " by "
    o <- read <$> many1 digit
    return $ Col i o
