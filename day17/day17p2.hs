#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import Algorithm.Search
import Data.Digest.Pure.MD5
import qualified Data.ByteString.Lazy.Char8 as C8
import Data.Maybe
type Pos = (Int, Int)
type Path = String
type State = (Pos, Path)

main :: IO ()
main = interact $ show . fromJust . solve . head . lines

solve :: String -> Maybe (Int, [State])
solve key = dijkstra next cost isFinished start
    where
        next :: State -> [State]
        next ((x,y),path)= let (u:d:l:r:_) = show . md5 . C8.pack $ key ++ path
                               up    = if u `elem` "bcdef" && y > 1 then [((x,y - 1), path ++ "U")] else []
                               down  = if d `elem` "bcdef" && y < 4 then [((x,y + 1), path ++ "D")] else []
                               left  = if l `elem` "bcdef" && x > 1 then [((x - 1,y), path ++ "L")] else []
                               right = if r `elem` "bcdef" && x < 4 then [((x + 1,y), path ++ "R")] else []
                           in up ++ down ++ left ++ right

        cost :: State -> State -> Int
        cost ((1,1),"") _ = 1000
        cost __________ _ = -1

        isFinished :: State -> Bool
        isFinished ((4,4),_) = True
        isFinished _________ = False

        start :: State
        start = ((1,1),"")