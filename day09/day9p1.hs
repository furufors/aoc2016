#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
main :: IO ()
main = interact $ show . length . decompress . head . lines

decompress :: String -> String
decompress ('(':ss) = let inst  = takeWhile (/= ')') ss
                          rest  = drop 1 $ dropWhile (/= ')') ss
                          count = read $ takeWhile (`elem` ['0'..'9']) inst
                          reps  = read . drop 1 $ dropWhile (`elem` ['0'..'9']) inst
                      in (concat $ replicate reps (take count rest)) ++ (decompress (drop count rest))
decompress (s:ss) = s:(decompress ss)
decompress [] = []
