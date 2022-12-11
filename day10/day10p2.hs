#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import Text.Parsec
import Control.Parallel.Strategies
import Debug.Trace
import qualified Data.Map as M

type JustBot = Int
type Val = Int
type BotStatus = M.Map JustBot (Maybe Int, Maybe Int)
data BotOrOutput = Bot Int | Output Int deriving (Show)
data Action = Value Val JustBot | Gives JustBot BotOrOutput BotOrOutput deriving (Show)

main :: IO ()
main = interact $ show . step M.empty . parMap rpar parsein . lines

step :: BotStatus -> [Action] -> Int
step bs [] = error "Didn't find what you were looking for."
step bs ((Value v b):rest) =
    let bs' = case M.lookup b bs of
                (Just (Nothing, Nothing)) -> M.insert b (Just v, Nothing) bs
                (Just (Just l,  Nothing)) -> M.insert b (Just l,  Just v) bs
                Nothing                   -> M.insert b (Just v, Nothing) bs
                otherwise -> error "Trying to insert into full bot"
    in step bs' rest
step bs (hd@(Gives i (Bot low) (Bot high)):rest) = case M.lookup i bs of
    (Just (Just a, Just b))-> step bs ((Value (min a b) low):(Value (max a b) high):rest)
    otherwise -> step bs (rest ++ [hd]) -- Move to back
step bs (hd@(Gives i (Output low) (Bot high)):rest) = case M.lookup i bs of
    (Just (Just a, Just b))-> trace (show (low, min a b)) $ step bs ((Value (max a b) high):rest)
    otherwise -> step bs (rest ++ [hd]) -- Move to back
step bs (hd@(Gives i (Bot low) (Output high)):rest) = case M.lookup i bs of
    (Just (Just a, Just b))-> trace (show (high, max a b)) $ step bs ((Value (min a b) low):rest)
    otherwise -> step bs (rest ++ [hd]) -- Move to back
step bs (hd@(Gives i (Output low) (Output high)):rest) = case M.lookup i bs of
    (Just (Just a, Just b))-> trace (show [(high, max a b), (low, min a b)]) $ step bs rest
    otherwise -> step bs (rest ++ [hd]) -- Move to back

keyOfValue :: (Maybe Int, Maybe Int) -> [(JustBot,(Maybe Int, Maybe Int))] -> Maybe Int
keyOfValue _ [] = Nothing
keyOfValue a ((i,b):rest) = if a == b then Just i else keyOfValue a rest

parsein :: String -> Action
parsein input = case parse (try parseValue <|> try parseGives) "parsein" input of
    Left err -> error $ show err
    Right a -> a

parseValue :: Parsec String () Action
parseValue = do -- value 23 goes to bot 208
    ___ <- string "value "
    val <- read <$> many1 digit
    ___ <- string " goes to bot "
    bot <- read <$> many1 digit
    return $ Value val bot

parseGives :: Parsec String () Action
parseGives = do -- bot 125 gives low to bot 58 and high to bot 57
    ___ <- string "bot "
    id1 <- read <$> many1 digit
    ___ <- string " gives low to "
    id2 <- parseBotOrOutput
    ___ <- string " and high to "
    id3 <- parseBotOrOutput
    return $ Gives id1 id2 id3

parseBotOrOutput :: Parsec String () BotOrOutput
parseBotOrOutput = try parseBot <|> try parseOutput

parseBot ::  Parsec String () BotOrOutput
parseBot = do
    _ <- string "bot "
    i <- read <$> many1 digit
    return $ Bot i

parseOutput ::  Parsec String () BotOrOutput
parseOutput = do
    _ <- string "output "
    i <- read <$> many1 digit
    return $ Output i
