#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import Text.Parsec
import Data.List
import Data.Maybe
data Dir = L | R  deriving (Show)
data Inst = SwapPos Int Int
          | SwapLet Char Char
          | Reverse Int Int
          | Rotate Dir Int
          | Move Int Int
          | RotatePos Char deriving (Show)

secretkey = "fbgdceah"
indexOf e es = fromJust $ elemIndex e es

main :: IO ()
main = interact $ crack . map parsein . lines

crack :: [Inst] -> String
crack is = head . dropWhile (\p -> foldl gogogo p is /= secretkey) $ permute "abcdefgh"

permute :: String -> [String]
permute [] = [[]]
permute str = do
   x  <- str
   xs <- permute (delete x str)
   return (x:xs)

gogogo :: [Char] -> Inst -> [Char]
gogogo s (SwapPos ai bi) = let a = s!!ai; b = s!!bi in insertAt b ai . insertAt a bi $ s
gogogo s (SwapLet a b) = let ai = indexOf a s; bi = indexOf b s in insertAt b ai . insertAt a bi $ s
gogogo s (Reverse f t) = take f s ++ reverse (drop f $ take (t+1) s) ++ drop (t+1) s
gogogo s (Rotate L i) = drop i s ++ take i s
gogogo s (Rotate R i) = drop (length s - i) s ++ take (length s - i) s
gogogo s (Move f t) = let temp = take f s ++ drop (f+1) s
                      in take t temp ++ [s!!f] ++ drop t temp
gogogo s (RotatePos c) = let n = indexOf c s
                             tmp1 = gogogo s (Rotate R 1)
                             tmp2 = gogogo tmp1 (Rotate R n)
                         in if n >= 4
                            then gogogo tmp2 (Rotate R 1)
                            else tmp2

insertAt :: Char -> Int -> String -> String
insertAt c 0 s = c:(tail s)
insertAt c x s = head s:(insertAt c (x-1) (tail s))

parsein :: String -> Inst
parsein input = case parse (try parseSwapPos <|> try parseSwapLet <|> try parseReverse <|> try parseRotate <|> try parseMove <|> try parseRotatePos) "parsein" input of
    Left err -> error $ show err
    Right a -> a

parseSwapPos :: Parsec String () Inst
parseSwapPos = do
    _ <- string "swap position "
    x <- read <$> many1 digit
    _ <- string " with position "
    y <- read <$> many1 digit
    return $ SwapPos x y

parseSwapLet :: Parsec String () Inst
parseSwapLet = do
    _ <- string "swap letter "
    x <- anyChar
    _ <- string " with letter "
    y <- anyChar
    return $ SwapLet x y

parseReverse :: Parsec String () Inst
parseReverse = do
    _ <- string "reverse positions "
    x <- read <$> many1 digit
    _ <- string " through "
    y <- read <$> many1 digit
    return $ Reverse x y

parseRotate :: Parsec String () Inst
parseRotate = do
    _ <- string "rotate "
    d <- (const L <$> string "left ") <|> (const R <$> string "right ")
    x <- read <$> many1 digit
    return $ Rotate d x

parseMove :: Parsec String () Inst
parseMove = do
    _ <- string "move position "
    x <- read <$> many1 digit
    _ <- string " to position "
    y <- read <$> many1 digit
    return $ Move x y

parseRotatePos :: Parsec String () Inst
parseRotatePos = do
    _ <- string "rotate based on position of letter "
    x <- anyChar
    return $ RotatePos x