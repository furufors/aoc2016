#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import Text.Parsec hiding (count)
type Disc = (Int, Int)
type Discs = [Disc]

main :: IO ()
main = interact $ show . play . addEleventh . map parsein . lines

addEleventh :: Discs -> Discs
addEleventh discs = discs ++ [(11,0)]

play :: Discs -> Int
play discs = head $ filter (passes discs) [1..]

passes :: Discs -> Int -> Bool
passes discs start = all singlePass $ zip discs (map (+ start) [1..])

singlePass :: (Disc, Int) -> Bool
singlePass ((slots, start), time) = (start + time) `mod` slots == 0

parsein :: String -> Disc
parsein input = case parse (try parseDisc) "parse" input of
    Left err -> error $ show err
    Right a -> a

-- Disc #1 has 5 positions; at time=0, it is at position 4.
parseDisc :: Parsec String () Disc
parseDisc = do
    _     <- string "Disc #"
    _     <- many1 digit
    _     <- string " has "
    slots <- read <$> many1 digit
    _     <- string " positions; at time=0, it is at position "
    start <- read <$> many1 digit
    return (slots, start)