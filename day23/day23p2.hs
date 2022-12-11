#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import Data.List
import Debug.Trace

data Register = Val Int | A | B | C | D deriving Show
type Registers = (Int, Int, Int, Int)
type Instructions = [Instruction]
type Pointer = Int
data Instruction = Cpy Register Register
                 | Inc Register
                 | Dec Register
                 | Jnz Register Register
                 | Tgl Register
                 | Mul Register Register

{-
cpy x y copies x (either an integer or the value of a register) into register y.
inc x increases the value of register x by one.
dec x decreases the value of register x by one.
jnz x y jumps to an instruction y away (positive means forward; negative means backward), but only if x is not zero.
tgl x toggles the instruction x away (pointing at instructions like jnz does: positive means forward; negative means backward):
    For one-argument instructions, inc becomes dec, and all other one-argument instructions become inc.
    For two-argument instructions, jnz becomes cpy, and all other two-instructions become jnz.
    The arguments of a toggled instruction are not affected.
    If an attempt is made to toggle an instruction outside the program, nothing happens.
    If toggling produces an invalid instruction (like cpy 1 2) and an attempt is later made to execute that instruction, skip it instead.
    If tgl toggles itself (for example, if a is 0, tgl a would target itself and become inc a), the resulting instruction is not executed until the next time it is reached.
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
parse ('t':'g':'l':' ':ss) = Tgl (readR ss)
parse ('m':'u':'l':' ':ss) = let x = takeWhile (/=' ') ss
                                 y = tail $ dropWhile (/=' ') ss
                             in Mul (readR x) (readR y)

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
                        (rs', p', ins') = step rs p i ins
                    in run rs' p' ins'

step :: Registers -> Pointer -> Instruction -> Instructions -> (Registers, Pointer, Instructions)
step rs p _ ins | p == 5 || p == 21 = -- inc a, inc d, jnz d -2, inc c, jnz c -5 => a += (c*d)
    let aval = rd rs A
        cval = rd rs C
        dval = rd rs D
        rs1 = set rs A (aval + abs (cval*dval))
        rs2 = set rs1 C 0
        rs3 = set rs2 D 0
    in (rs3,p+4,ins)
step rs p (Cpy x (Val _)) ins = (rs, p + 1, ins) -- Invalid instruction -> Skip
step rs p (Cpy x y) ins = (cpy rs x y, p + 1, ins)
step rs p (Inc x)   ins = (inc rs x, p + 1, ins)
step rs p (Dec x)   ins = (dec rs x, p + 1, ins)
step rs p (Jnz x y) ins = (rs, if rd rs x == 0 then p + 1 else p + rd rs y, ins)
step rs p (Mul x y) ins = (mul rs x y, p + 1, ins)
step rs p (Tgl x)   ins =
    let ip = p + rd rs x
        ins' = if ip `elem` [0..(length ins - 1)] then take ip ins ++ [toggle (ins!!ip)] ++ drop (ip + 1) ins else ins
    in (rs, p + 1, ins')

toggle :: Instruction -> Instruction
toggle (Cpy x y) = Jnz x y
toggle (Inc x)   = Dec x
toggle (Dec x)   = Inc x
toggle (Jnz x y) = Cpy x y
toggle (Tgl x)   = Inc x

-- Sets register y to value of register x
cpy :: Registers -> Register -> Register -> Registers
cpy rs x y = let xval = rd rs x
             in set rs y xval

mul :: Registers -> Register -> Register -> Registers
mul rs x y = let xval = (rd rs x) * (rd rs y)
             in set (set rs x xval) y 0

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
emptyReg = (12,0,0,0)