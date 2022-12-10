#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import Data.Sequence (Seq, (<|), (|>), (><))
import qualified Data.Sequence as Seq

main :: IO ()
main = interact $ show . game . read . head . lines

game :: Int -> Int
game n = gogogo $ Seq.fromList [1..n]

gogogo :: Seq Int -> Int
gogogo (id Seq.:<| Seq.Empty) = id
gogogo rest = let half  = (Seq.length rest) `div` 2
                  left  = Seq.take half rest
                  right = Seq.drop (half + 1) rest
                  new = left >< right
              in gogogo $ (Seq.drop 1 new) >< (Seq.take 1 new)