#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import Data.List

data Register = Val Int | A | B | C | D
type Registers = (Int, Int, Int, Int)
type Instructions = [Instruction]
type Pointer = Int
data Instruction = Cpy Register Register
                 | Inc Register
                 | Dec Register
                 | Jnz Register Register

{-
cpy x y copies x (either an integer or the value of a register) into register y.
inc x increases the value of register x by one.
dec x decreases the value of register x by one.
jnz x y jumps to an instruction y away (positive means forward; negative means backward), but only if x is not zero.
-}

main :: IO ()
main = interact $ show . getA . run emptyReg 0 . map parse. lines

getA :: Registers -> Int
getA (a, _, _, _) = a

parse :: String -> Instruction
parse ('c':'p':'y':' ':ss) = let x = takeWhile (/=' ') ss
                                 y = tail $ dropWhile (/=' ') ss
                             in Cpy (readR x) (readR y)
parse ('i':'n':'c':' ':ss) = Inc (readR ss)
parse ('d':'e':'c':' ':ss) = Dec (readR ss)
parse ('j':'n':'z':' ':ss) = let x = takeWhile (/=' ') ss
                                 y = tail $ dropWhile (/=' ') ss
                             in Jnz (readR x) (readR y)

readR :: String -> Register
readR ss = case ss of
             "a" -> A
             "b" -> B
             "c" -> C
             "d" -> D
             otherwise -> Val (read ss)

run :: Registers -> Pointer -> Instructions -> Registers
run rs p ins = if p >= length ins
               then rs
               else let i = ins !! p
                        (rs', p') = step rs p i
                    in run rs' p' ins

step :: Registers -> Pointer -> Instruction -> (Registers, Pointer)
step rs p (Cpy x y) = (cpy rs x y, p + 1)
step rs p (Inc x)   = (inc rs x, p + 1)
step rs p (Dec x)   = (dec rs x, p + 1)
step rs p (Jnz x y) = (rs, if rd rs x == 0 then p + 1 else p + rd rs y)

-- Sets register y to value of register x
cpy :: Registers -> Register -> Register -> Registers
cpy rs x y = let xval = rd rs x
             in set rs y xval

inc :: Registers -> Register -> Registers
inc rs x = let xval = rd rs x
           in set rs x (xval + 1)

dec :: Registers -> Register -> Registers
dec rs x = let xval = rd rs x
           in set rs x (xval - 1)

rd :: Registers -> Register -> Int
rd (a,b,c,d) (Val x) = x
rd (a,b,c,d) A = a
rd (a,b,c,d) B = b
rd (a,b,c,d) C = c
rd (a,b,c,d) D = d

set :: Registers -> Register -> Int -> Registers
set (a,b,c,d) A x = (x,b,c,d)
set (a,b,c,d) B x = (a,x,c,d)
set (a,b,c,d) C x = (a,b,x,d)
set (a,b,c,d) D x = (a,b,c,x)

emptyReg :: Registers
emptyReg = (0,0,1,0)