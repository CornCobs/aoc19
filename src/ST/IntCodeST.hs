module ST.IntCodeST where

import qualified Data.Vector.Mutable as MV 
import Data.Functor ((<&>))
import Control.Monad
import Control.Monad.ST -- cannot use ST.Lazy because no instance of PrimMonad for ST.Lazy. If want lazy ST can use MArray instead
import Control.Monad.ST.Unsafe (unsafeInterleaveST)

import Control.Monad.Trans.Reader
import Control.Monad.Trans.Class

type IntCode s a = ReaderT (MV.STVector s Integer) (ST s) a 

intcode :: [Integer] -> [Integer] -> [Integer]
intcode opcodes input = runST $ do 
  program <- MV.replicate 10000 0
  forM_ (zip [0..] opcodes) $ \(ix, opcode) -> MV.write program ix opcode
  runReaderT (process 0 0 input) program 
  
process :: Int -> Int -> [Integer] -> IntCode s [Integer]
process relbase pointer input = do 
  code <- readMemory pointer 
  case code `mod` 100 of 
    1 -> mathOp (+) relbase code pointer >> process relbase (pointer + 4) input
    2 -> mathOp (*) relbase code pointer >> process relbase (pointer + 4) input
    3 -> inputOp (head input) relbase code pointer >> process relbase (pointer + 2) (tail input) 
    4 -> outputOp relbase code pointer >>= \out -> (out :) <$> (mapReaderT unsafeInterleaveST (process relbase (pointer + 2) input))
    5 -> jumpOp (/= 0) relbase code pointer >>= \pointer' -> process relbase pointer' input 
    6 -> jumpOp (== 0) relbase code pointer >>= \pointer' -> process relbase pointer' input 
    7 -> compOp (<)  relbase code pointer >> process relbase (pointer + 4) input 
    8 -> compOp (==) relbase code pointer >> process relbase (pointer + 4) input 
    9 -> relBaseOffsetOp relbase code pointer >>= \newBase -> process newBase (pointer + 2) input 
    99 -> return []
    _ -> error "Invalid Code"
  
mathOp :: (Integer -> Integer -> Integer) -> Int -> Integer -> Int -> IntCode s ()
mathOp op relbase code pointer = do 
  num1 <- getValue 1 relbase code pointer
  num2 <- getValue 2 relbase code pointer 
  target <- getTargetPosition 3 relbase code pointer 
  writeMemory target (num1 `op` num2)

compOp :: (Integer -> Integer -> Bool) -> Int -> Integer -> Int -> IntCode s ()
compOp op = mathOp (\num1 num2 -> if num1 `op` num2 then 1 else 0) 

jumpOp :: (Integer -> Bool) -> Int -> Integer -> Int -> IntCode s Int
jumpOp jumpCond relbase code pointer = do 
  cond <- getValue 1 relbase code pointer
  if jumpCond cond 
  then fromIntegral <$> getValue 2 relbase code pointer
  else return (pointer + 3)
        
relBaseOffsetOp :: Int -> Integer -> Int -> IntCode s Int 
relBaseOffsetOp relbase code pointer = do 
  offset <- getValue 1 relbase code pointer
  return (relbase + fromIntegral offset)
  
inputOp :: Integer -> Int -> Integer -> Int -> IntCode s () 
inputOp input relbase code pointer = getTargetPosition 1 relbase code pointer >>= \pos -> writeMemory pos input

outputOp :: Int -> Integer -> Int -> IntCode s Integer 
outputOp = getValue 1
  
getTargetPosition :: Int -> Int -> Integer -> Int -> IntCode s Int 
getTargetPosition offset relbase code pointer = 
  case (code `mod` (10^(offset+2))) `div` (10^(offset+1)) of 
    1 -> return (pointer + offset)
    0 -> readMemory (pointer + offset) <&> fromIntegral
    2 -> readMemory (pointer + offset) <&> (+ relbase) . fromIntegral
    
getValue :: Int -> Int -> Integer -> Int -> IntCode s Integer 
getValue offset relbase code pointer = getTargetPosition offset relbase code pointer >>= readMemory
  
readMemory :: Int -> IntCode s Integer
readMemory pointer = ask >>= \program -> lift $ MV.read program pointer
  
writeMemory :: Int -> Integer -> IntCode s ()
writeMemory pointer val = ask >>= \program -> lift $ MV.write program pointer val 