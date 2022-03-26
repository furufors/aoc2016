#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
main :: IO ()
main = interact $ unlines . map (show . decompress) . lines

decompress :: String -> Int
decompress ('(':ss) = let inst  = takeWhile (/= ')') ss
                          rest  = drop 1 $ dropWhile (/= ')') ss
                          count = read $ takeWhile (`elem` ['0'..'9']) inst
                          reps  = read . drop 1 $ dropWhile (`elem` ['0'..'9']) inst
                      in reps * (decompress $ take count rest) + (decompress (drop count rest))
decompress (s:ss) = 1 + decompress ss
decompress [] = 0
