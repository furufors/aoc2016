#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import Data.List.Split (splitWhen)

main :: IO ()
main = interact $ show . length . filter (id) . map parseIP . lines

parseIP :: String -> Bool
parseIP s = let (outside, inside) = inAndOut s
            in any hasABBA inside && (not . any hasABBA) outside

inAndOut :: String -> ([String], [String])
inAndOut s = let pieces = splitWhen (\a -> a == '[' || a == ']') s
                 odds   = [pieces!!i | i <- [0..(length pieces - 1)], odd i]
                 evens  = [pieces!!i | i <- [0..(length pieces - 1)], even i]
             in (odds, evens)

hasABBA :: String -> Bool
hasABBA ([])         = False
hasABBA (a:[])       = False
hasABBA (a:b:[])     = False
hasABBA (a:b:c:[])   = False
hasABBA (a:b:c:d:es) = (a == d && b == c && a /= b) || hasABBA (b:c:d:es)
