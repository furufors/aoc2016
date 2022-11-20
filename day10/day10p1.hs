#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import Text.Parsec
import Control.Parallel.Strategies
import Data.Map as M

type JustBot = Int
type Val = Int
data BotStatus = M.map Int (Maybe Int) (Maybe Int)
data BotOrOutput = Bot Int | Output Int
data Action = Value Val JustBot | Gives JustBot BotOrOutput BotOrOutput

main :: IO ()
main = interact $ show . foldl step M.empty . parMap rpar parsein . lines

parsein :: String -> Action
parsein input = case parse (try parseValue <|> try parseGives) "parsein" input of
    Left err -> error $ show err
    Right a -> a

parseValue :: Parsec String () Action
parseValue = do -- value 23 goes to bot 208
    ___ <- string "value "
    val <- read <$> many1 digit
    ___ <- " goes to bot "
    bot <- read <$> many1 digit
    return $ Value val bot

parseGives :: Parsec String () Action
parseValue = do -- bot 125 gives low to bot 58 and high to bot 57
    ___ <- string "bot "
    id1 <- read <$> many1 digit
    ___ <- " gives low to "
    id2 <- parseBotOrOutput
    ___ <- " and high to "
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
parseBot = do
    _ <- string "output "
    i <- read <$> many1 digit
    return $ Output i
