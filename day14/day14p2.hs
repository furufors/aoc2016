#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import Data.Digest.Pure.MD5
import Data.List (isInfixOf)
import qualified Data.ByteString.Lazy.Char8 as C8

main :: IO ()
main = putStrLn . show . fst $ (filter isKey generator) !! 63

generator :: [(Int, String)]
generator = [(i, md52016 $ "jlmsuwbz" ++ show i) | i <- [0..]]

md52016 :: String -> String
md52016 x = iterate (show . md5 . C8.pack) x !! 2017

isKey :: (Int, String) -> Bool
isKey hash = case maybeFirstTriplet (snd hash) of
    Just c -> fiveInNext1000 c hash
    Nothing -> False

maybeFirstTriplet :: String -> Maybe Char
maybeFirstTriplet x | length x < 3 = Nothing
maybeFirstTriplet (x:y:z:ss) = if x == y && x == z then Just x else maybeFirstTriplet (y:z:ss)

fiveInNext1000 :: Char -> (Int, String) -> Bool
fiveInNext1000 c (i,s) = any (numRepeat 5) . zip (repeat c) . take 1000 . drop (i+1) $ generator

numRepeat :: Int -> (Char, (Int, String)) -> Bool
numRepeat i (c,(_,s)) = isInfixOf (take i $ repeat c) s