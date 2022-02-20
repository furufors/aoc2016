#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import Data.List (intersperse, group, sort, sortBy)
import Data.List.Split (splitWhen)
import Data.Char (chr, ord)

main :: IO ()
main = interact $ concat . intersperse "\n" . filter (/="") . map valid . map parsein . lines

valid :: (String, String, Int) -> String
valid (a,b,c) = let groups = group . sort . filter (not . (`elem` ['0'..'9'])) . filter (/='-') $ a
                    groupCount = map (\a -> (length a, head a)) groups
                    -- Primary higher number, secondary letter order
                    sorted = sortBy (\a b -> fst b `compare` fst a <> snd a `compare` snd b) groupCount
                    letterFreq = map snd sorted
                in if take 5 letterFreq == b
                    then concatMap (rotate c) (init . splitWhen (=='-') $ a) ++ " - " ++ show c
                    else ""

rotate :: Int -> String -> String
rotate n s = map (\c -> chr $ ord 'a' + (ord c - ord 'a' + n) `mod` 26) s

parsein :: String -> (String, String, Int)
parsein s = let (a,b) = span (/='[') s
                is = filter (`elem` ['0'..'9']) a
            in (a, take 5 . drop 1 $ b, read is)
