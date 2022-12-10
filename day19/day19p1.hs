#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import Data.Sequence

main :: IO ()
main = interact $ show . game . read . head . lines

game :: Int -> Int
game n = gogogo . fromList $ Prelude.zip [1..n] (repeat 1)

gogogo :: Seq (Int,Int) -> Int
gogogo ((id,n):<|Empty) = id
gogogo ((id1,n1):<|(_,n2):<|rest) = gogogo $ rest |> (id1,n1+n2)