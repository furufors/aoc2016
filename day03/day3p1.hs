#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
main :: IO ()
main = interact $ show . length . filter id . map (valid . parsein) . lines

parsein :: String -> (Int, Int, Int)
parsein s = let (a:b:c:_) = map read . words $ s
            in (a,b,c)

valid :: (Int, Int, Int) -> Bool
valid (a,b,c) =    a + b > c
                && a + c > b
                && b + c > a
