{-# LANGUAGE TypeApplications #-}

module Day9 where

import Data.List
import Control.Monad (forM_)

import Data.Vector (fromList, unsafeThaw)
import qualified Data.Vector.Mutable as MV

main = day9

diagnostic :: [Integer] -> IO ()
diagnostic opcodes = do
  program <- MV.replicate 10000 0
  forM_ (zip [0..] opcodes) $ \(ix, opcode) -> MV.write program ix opcode
  let process relbase pos = do 
        code <- MV.read program pos 
        case code `mod` 100 of 
          1 -> mathOp (+) relbase code pos program >> process relbase (pos + 4)
          2 -> mathOp (*) relbase code pos program >> process relbase (pos + 4)
          3 -> inputOp    relbase code pos program >> process relbase (pos + 2)
          4 -> outputOp   relbase code pos program >> process relbase (pos + 2)
          5 -> jumpOp (/= 0) relbase code pos program >>= process relbase 
          6 -> jumpOp (== 0) relbase code pos program >>= process relbase
          7 -> compOp (<)  relbase code pos program >> process relbase (pos + 4)
          8 -> compOp (==) relbase code pos program >> process relbase (pos + 4)
          9 -> relBaseOffsetOp relbase code pos program >>= (\newBase -> process newBase (pos + 2))
          99 -> return ()
  process 0 0

getVal :: Int -> Integer -> Integer -> Int -> MV.IOVector Integer -> IO Integer
getVal offset relbase code pos program = 
  case (code `mod` (10^(offset+2))) `div` (10^(offset+1)) of 
    0 -> fromIntegral <$> MV.read program (pos + offset) >>= MV.read program 
    1 -> MV.read program (pos + offset)
    2 -> MV.read program (pos + offset) >>= \relpos -> MV.read program (fromIntegral (relpos + relbase))

relBaseOffsetOp :: Integer -> Integer -> Int -> MV.IOVector Integer -> IO Integer 
relBaseOffsetOp relbase code pos program = do 
  offset <- getVal 1 relbase code pos program 
  --putStrLn $ "code: " ++ show code ++ "relbase: " ++ show relbase ++ "pos: " ++ show pos ++ "offset: " ++ show offset
  return (relbase + offset)

mathOp :: (Integer -> Integer -> Integer) -> Integer -> Integer -> Int -> MV.IOVector Integer -> IO ()
mathOp op relbase code pos program = do 
  num1 <- getVal 1 relbase code pos program 
  num2 <- getVal 2 relbase code pos program 
  targetPos <- if (code `mod` 100000) `div` 10000 == 0
               then fromIntegral <$> MV.read program (pos + 3)
               else (fromIntegral . (+relbase)) <$> MV.read program (pos + 3)
  --putStrLn $ "code: " ++ show code ++ "relbase: " ++ show relbase ++ "pos: " ++ show pos ++ "num1 num2: " ++ show num1 ++ " " ++ show num2 ++ "targetPos: " ++ show targetPos
  MV.write program targetPos (op num1 num2)
  
inputOp :: Integer -> Integer -> Int -> MV.IOVector Integer -> IO ()
inputOp relbase code pos program = do 
  input <- read <$> getLine
  targetPos <- if (code `mod` 1000) `div` 100 == 0 
               then fromIntegral <$> MV.read program (pos + 1)
               else (fromIntegral . (+relbase)) <$> MV.read program (pos + 1)
  --putStrLn $ "code: " ++ show code ++ "relbase: " ++ show relbase ++ "pos: " ++ show pos ++ "targetPos: " ++ show targetPos
  MV.write program targetPos input
  
outputOp :: Integer -> Integer -> Int -> MV.IOVector Integer -> IO ()
outputOp relbase code pos program = 
  --putStrLn $ "code: " ++ show code ++ "relbase: " ++ show relbase ++ "pos: " ++ show pos
  getVal 1 relbase code pos program >>= print

jumpOp :: (Integer -> Bool) -> Integer -> Integer -> Int -> MV.IOVector Integer -> IO Int 
jumpOp jumpCond relbase code pos program = do 
  cond <- getVal 1 relbase code pos program 
  --putStrLn $ "code: " ++ show code ++ "relbase: " ++ show relbase ++ "pos: " ++ show pos ++ "cond: " ++ show cond
  if jumpCond cond 
  then fromIntegral <$> getVal 2 relbase code pos program
  else return (pos + 3)
      
compOp :: (Integer -> Integer -> Bool) -> Integer -> Integer -> Int -> MV.IOVector Integer -> IO ()
compOp op = mathOp (\num1 num2 -> if num1 `op` num2 then 1 else 0)  

day9 :: IO ()
day9 = do 
  inputString <- readFile "day9.txt"
  let opcodes = map (read @Integer) $ words [if c == ',' then ' ' else c | c <- inputString]
  diagnostic opcodes