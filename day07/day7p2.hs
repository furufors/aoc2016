#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import Data.List.Split (splitWhen)
type Pair = (Char, Char)

main :: IO ()
main = interact $ show . length . filter (id) . map parseIP . lines

parseIP :: String -> Bool
parseIP s = let (outside, inside) = inAndOut s
                abas = foldl hasABA [] outside
                babs = foldl hasABA [] inside
                matches = [() | (a,b) <- abas, elem (b,a) babs]
            in length matches > 0

inAndOut :: String -> ([String], [String])
inAndOut s = let pieces = splitWhen (\a -> a == '[' || a == ']') s
                 odds   = [pieces!!i | i <- [0..(length pieces - 1)], odd i]
                 evens  = [pieces!!i | i <- [0..(length pieces - 1)], even i]
             in (odds, evens)

hasABA :: [Pair] -> String -> [Pair]
hasABA ps ([])       = ps
hasABA ps (a:[])     = ps
hasABA ps (a:b:[])   = ps
hasABA ps (a:b:c:es) = if (a == c && a /= b)
                       then (a,b):(hasABA ps (b:c:es))
                       else hasABA ps (b:c:es)
