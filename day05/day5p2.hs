#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import Data.Digest.Pure.MD5
import qualified Data.ByteString.Lazy.Char8 as C8
import Data.List (sortOn)

main :: IO ()
main = putStrLn . map ((\s -> s !! 6) . fst). sortOn snd . take 8 . uniques . filter isKey $ generator

generator :: [(String, Int)]
generator = [(s, read [s !! 5]) | i <- [0..], let s = show . md5 . C8.pack $ "uqwqemis" ++ show i, (s !! 5) `elem` "01234567"]

isKey :: (String, Int) -> Bool
isKey (('0':'0':'0':'0':'0':_),_) = True
isKey _ = False

uniques :: [(String, Int)] -> [(String, Int)]
uniques ((s,i):es) = (s,i):(uniques (remove i es))
uniques [        ] = []

remove :: Int -> [(String, Int)] -> [(String, Int)]
remove i1 ((s,i2):es) = if i1 == i2 then remove i1 es else (s,i2):(remove i1 es)
remove _  [         ] = []