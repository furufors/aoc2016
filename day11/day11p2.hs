#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

import Algorithm.Search
import Data.List
import Data.Maybe

type State = (Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int)
-- Elevator, HG, HM, LG, LM
-- hydrogen = elerium
-- lithium = dilithium
-- strontium
-- plutonium
-- thulium
-- ruthenium
-- curium

main :: IO ()
main = interact $ show . fromJust . solve . toState (1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0) . zip [1..] . lines

toState :: State -> [(Int, String)] -> State
toState st [] = st
toState (f,hg,hm,lg,lm,sg,sm,pg,pm,tg,tm,rg,rm,cg,cm) ((i,ss):xs)
    = let hg' = if isInfixOf "hydrogen generator" ss then i else hg
          hm' = if isInfixOf "hydrogen-compatible microchip" ss then i else hm
          lg' = if isInfixOf "lithium generator" ss then i else lg
          lm' = if isInfixOf "lithium-compatible microchip" ss then i else lm
          sg' = if isInfixOf "strontium generator" ss then i else sg
          sm' = if isInfixOf "strontium-compatible microchip" ss then i else sm
          pg' = if isInfixOf "plutonium generator" ss then i else pg
          pm' = if isInfixOf "plutonium-compatible microchip" ss then i else pm
          tg' = if isInfixOf "thulium generator" ss then i else tg
          tm' = if isInfixOf "thulium-compatible microchip" ss then i else tm
          rg' = if isInfixOf "ruthenium generator" ss then i else rg
          rm' = if isInfixOf "ruthenium-compatible microchip" ss then i else rm
          cg' = if isInfixOf "curium generator" ss then i else cg
          cm' = if isInfixOf "curium-compatible microchip" ss then i else cm
      in toState (f,hg',hm',lg',lm',sg',sm',pg',pm',tg',tm',rg',rm',cg',cm') xs

solve :: State -> Maybe (Int, [State])
solve start = dijkstra next cost solved start
    where
        next :: State -> [State]
        next s = filter isValid . nub $ (moveUp s ++ moveDown s)

        cost :: State -> State -> Int
        cost _ _ = 1

        solved :: State -> Bool
        solved s = s == (4,4,4,4,4,4,4,4,4,4,4,4,4,4,4)

        isValid :: State -> Bool
        isValid (f,hg,hm,lg,lm,sg,sm,pg,pm,tg,tm,rg,rm,cg,cm) =
            (not (any (==hm) [   lm,sm,pm,tm,rm,cm] && hm /= hg)) &&
            (not (any (==lm) [hm,   sm,pm,tm,rm,cm] && lm /= lg)) &&
            (not (any (==sm) [hm,lm,   pm,tm,rm,cm] && sm /= sg)) &&
            (not (any (==pm) [hm,lm,sm,   tm,rm,cm] && pm /= pg)) &&
            (not (any (==tm) [hm,lm,sm,pm,   rm,cm] && tm /= tg)) &&
            (not (any (==rm) [hm,lm,sm,pm,tm,   cm] && rm /= rg)) &&
            (not (any (==cm) [hm,lm,sm,pm,tm,rm   ] && cm /= cg))

        moveUp :: State -> [State]
        moveUp (4,_,_,_,_,_,_,_,_,_,_,_,_,_,_) = [] -- Cannot move up from top floor
        moveUp s = applyMove 1 s [] possibleMoves

        moveDown :: State -> [State]
        moveDown (1,_,_,_,_,_,_,_,_,_,_,_,_,_,_) = [] -- Cannot move down from ground floor
        moveDown s = applyMove (-1) s [] possibleMoves

        possibleMoves :: [[Int]]
        possibleMoves = [[x] | x <- [1..14]] ++ [[x,y] | x <- [1..14], y <- [x..14], x/=y]

        hasMovedSomething :: State -> State -> Bool
        hasMovedSomething (_,hg,hm,lg,lm,sg,sm,pg,pm,tg,tm,rg,rm,cg,cm) (_,hg',hm',lg',lm',sg',sm',pg',pm',tg',tm',rg',rm',cg',cm') =
            let a = (hg ,hm ,lg ,lm ,sg ,sm ,pg ,pm ,tg ,tm ,rg ,rm ,cg ,cm )
                b = (hg',hm',lg',lm',sg',sm',pg',pm',tg',tm',rg',rm',cg',cm')
            in a /= b

        applyMove :: Int -> State -> [State] -> [[Int]] -> [State]
        applyMove d s ss [] = filter (hasMovedSomething s) ss
        applyMove d s ss (x:xs) = applyMove d s ((helper s x):ss) xs
            where
                helper :: State -> [Int] -> State
                helper (f,hg,hm,lg,lm,sg,sm,pg,pm,tg,tm,rg,rm,cg,cm) [] = (f + d,hg,hm,lg,lm,sg,sm,pg,pm,tg,tm,rg,rm,cg,cm) -- Move elevator
                helper (f,hg,hm,lg,lm,sg,sm,pg,pm,tg,tm,rg,rm,cg,cm) (x:xs) =
                    let condMove = \item -> if item == f then item + d else item
                    in case x of
                        1  -> helper (f,condMove hg,hm,lg,lm,sg,sm,pg,pm,tg,tm,rg,rm,cg,cm) xs
                        2  -> helper (f,hg,condMove hm,lg,lm,sg,sm,pg,pm,tg,tm,rg,rm,cg,cm) xs
                        3  -> helper (f,hg,hm,condMove lg,lm,sg,sm,pg,pm,tg,tm,rg,rm,cg,cm) xs
                        4  -> helper (f,hg,hm,lg,condMove lm,sg,sm,pg,pm,tg,tm,rg,rm,cg,cm) xs
                        5  -> helper (f,hg,hm,lg,lm,condMove sg,sm,pg,pm,tg,tm,rg,rm,cg,cm) xs
                        6  -> helper (f,hg,hm,lg,lm,sg,condMove sm,pg,pm,tg,tm,rg,rm,cg,cm) xs
                        7  -> helper (f,hg,hm,lg,lm,sg,sm,condMove pg,pm,tg,tm,rg,rm,cg,cm) xs
                        8  -> helper (f,hg,hm,lg,lm,sg,sm,pg,condMove pm,tg,tm,rg,rm,cg,cm) xs
                        9  -> helper (f,hg,hm,lg,lm,sg,sm,pg,pm,condMove tg,tm,rg,rm,cg,cm) xs
                        10 -> helper (f,hg,hm,lg,lm,sg,sm,pg,pm,tg,condMove tm,rg,rm,cg,cm) xs
                        11 -> helper (f,hg,hm,lg,lm,sg,sm,pg,pm,tg,tm,condMove rg,rm,cg,cm) xs
                        12 -> helper (f,hg,hm,lg,lm,sg,sm,pg,pm,tg,tm,rg,condMove rm,cg,cm) xs
                        13 -> helper (f,hg,hm,lg,lm,sg,sm,pg,pm,tg,tm,rg,rm,condMove cg,cm) xs
                        14 -> helper (f,hg,hm,lg,lm,sg,sm,pg,pm,tg,tm,rg,rm,cg,condMove cm) xs
                        otherwise -> error $ "Unexpected input to helper " ++ show x
