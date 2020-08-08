module IntCode where

import qualified Data.Vector.Mutable as MV 
import Data.Functor ((<&>))
import Control.Monad

import Control.Monad.Trans.Reader
import Control.Monad.Trans.Class

type IntCode a = ReaderT (MV.IOVector Integer) IO a 

intcode :: [Integer] -> [Integer] -> IO [Integer]
intcode opcodes input = do 
  program <- MV.replicate 10000 0
  forM_ (zip [0..] opcodes) $ \(ix, opcode) -> MV.write program ix opcode
  runReaderT (process 0 0 input []) program 
  
process :: Int -> Int -> [Integer] -> [Integer] -> IntCode [Integer]
process relbase pointer input output = do 
  code <- readMemory pointer 
  case code `mod` 100 of 
    1 -> mathOp (+) relbase code pointer >> process relbase (pointer + 4) input output
    2 -> mathOp (*) relbase code pointer >> process relbase (pointer + 4) input output
    3 -> inputOp (head input) relbase code pointer >> process relbase (pointer + 2) (tail input) output
    4 -> outputOp relbase code pointer >>= \out -> process relbase (pointer + 2) input (out:output)
    5 -> jumpOp (/= 0) relbase code pointer >>= \pointer' -> process relbase pointer' input output
    6 -> jumpOp (== 0) relbase code pointer >>= \pointer' -> process relbase pointer' input output
    7 -> compOp (<)  relbase code pointer >> process relbase (pointer + 4) input output
    8 -> compOp (==) relbase code pointer >> process relbase (pointer + 4) input output
    9 -> relBaseOffsetOp relbase code pointer >>= \newBase -> process newBase (pointer + 2) input output
    99 -> return output
    _ -> error "Invalid Code"
  
mathOp :: (Integer -> Integer -> Integer) -> Int -> Integer -> Int -> IntCode ()
mathOp op relbase code pointer = do 
  num1 <- getValue 1 relbase code pointer
  num2 <- getValue 2 relbase code pointer 
  target <- getTargetPosition 3 relbase code pointer 
  writeMemory target (num1 `op` num2)

compOp :: (Integer -> Integer -> Bool) -> Int -> Integer -> Int -> IntCode ()
compOp op = mathOp (\num1 num2 -> if num1 `op` num2 then 1 else 0) 

jumpOp :: (Integer -> Bool) -> Int -> Integer -> Int -> IntCode Int
jumpOp jumpCond relbase code pointer = do 
  cond <- getValue 1 relbase code pointer
  if jumpCond cond 
  then fromIntegral <$> getValue 2 relbase code pointer
  else return (pointer + 3)
        
relBaseOffsetOp :: Int -> Integer -> Int -> IntCode Int 
relBaseOffsetOp relbase code pointer = do 
  offset <- getValue 1 relbase code pointer
  return (relbase + fromIntegral offset)
  
inputOp :: Integer -> Int -> Integer -> Int -> IntCode () 
inputOp input relbase code pointer = getTargetPosition 1 relbase code pointer >>= \pos -> writeMemory pos input

outputOp :: Int -> Integer -> Int -> IntCode Integer 
outputOp = getValue 1
  
getTargetPosition :: Int -> Int -> Integer -> Int -> IntCode Int 
getTargetPosition offset relbase code pointer = 
  case (code `mod` (10^(offset+2))) `div` (10^(offset+1)) of 
    1 -> return (pointer + offset)
    0 -> readMemory (pointer + offset) <&> fromIntegral
    2 -> readMemory (pointer + offset) <&> (+ relbase) . fromIntegral
    
getValue :: Int -> Int -> Integer -> Int -> IntCode Integer 
getValue offset relbase code pointer = getTargetPosition offset relbase code pointer >>= readMemory
  
readMemory :: Int -> IntCode Integer
readMemory pointer = ask >>= \program -> lift $ MV.read program pointer
  
writeMemory :: Int -> Integer -> IntCode ()
writeMemory pointer val = ask >>= \program -> lift $ MV.write program pointer val 