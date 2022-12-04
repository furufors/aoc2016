#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import Data.Digest.Pure.MD5
import qualified Data.ByteString.Lazy.Char8 as C8
type Pos = (Int, Int)
type Path = String
type State = (Pos, Path)

main :: IO ()
main = interact $ show . solve ((1,1),"") . head . lines

solve :: State -> String -> Int
solve ((4,4),path) key = length path
solve ((x,y),path) key = let (u:d:l:r:_) = show . md5 . C8.pack $ key ++ path
                             up    = if u `elem` "bcdef" && y > 1 then [((x,y - 1), path ++ "U")] else []
                             down  = if d `elem` "bcdef" && y < 4 then [((x,y + 1), path ++ "D")] else []
                             left  = if l `elem` "bcdef" && x > 1 then [((x - 1,y), path ++ "L")] else []
                             right = if r `elem` "bcdef" && x < 4 then [((x + 1,y), path ++ "R")] else []
                         in maximum . (0:) . map (\s -> solve s key) $ up ++ down ++ left ++ right