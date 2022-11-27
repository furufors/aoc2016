#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import Data.Digest.Pure.MD5
import qualified Data.ByteString.Lazy.Char8 as C8

main :: IO ()
main = putStrLn . show . map (\s -> s !! 5) . take 8 . filter isKey $ generator

generator :: [String]
generator = [show . md5 . C8.pack $ "uqwqemis" ++ show i | i <- [0..]]

isKey :: String -> Bool
isKey ('0':'0':'0':'0':'0':_) = True
isKey _ = False
