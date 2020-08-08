{-# LANGUAGE TypeApplications #-}

module Day7 where

import Data.List (unfoldr, permutations)
import Control.Monad (forM)

import Data.Vector (fromList, thaw)
import qualified Data.Vector.Mutable as MV
import Data.IORef

import Control.Monad.Trans.Maybe

feedbackLoop :: [Int] -> [Int] -> IO Int
feedbackLoop opcodes settings = do 
  ampA <- makeAmplifier opcodes (settings !! 0)
  ampB <- makeAmplifier opcodes (settings !! 1)
  ampC <- makeAmplifier opcodes (settings !! 2)
  ampD <- makeAmplifier opcodes (settings !! 3)
  ampE <- makeAmplifier opcodes (settings !! 4)
  let run input = do 
        out <- runMaybeT $ ampA input >>= ampB >>= ampC >>= ampD >>= ampE 
        case out of 
          Nothing -> return input
          Just nextInput -> run nextInput 
  run 0

makeAmplifier :: [Int] -> Int -> IO (Int -> MaybeT IO Int)
makeAmplifier opcodes setting = do 
  program <- thaw (fromList opcodes) 
  posref <- newIORef 0 :: IO (IORef Int)
  let amp runOnce input = MaybeT $ do
          let process pos = do
                code <- MV.read program pos 
                case code `mod` 100 of 
                  1 -> mathOp (+) code pos program >> process (pos + 4)
                  2 -> mathOp (*) code pos program >> process (pos + 4)
                  3 -> inputOp input code pos program >> if runOnce then return (pos + 2, Nothing) else process (pos + 2)
                  4 -> outputOp   code pos program
                  5 -> jumpOp (/= 0) code pos program >>= process 
                  6 -> jumpOp (== 0) code pos program >>= process 
                  7 -> compOp (<)  code pos program >> process (pos + 4)
                  8 -> compOp (==) code pos program >> process (pos + 4)
                  99 -> return (pos, Nothing)
          posval <- readIORef posref
          (nextPos, ret) <- process posval
          writeIORef posref nextPos 
          return ret
  runMaybeT $ amp True setting 
  return $ amp False

mathOp :: (Int -> Int -> Int) -> Int -> Int -> MV.IOVector Int -> IO ()
mathOp op code pos program = do 
  num1 <- if (code `mod` 1000)  `div` 100  == 0 then MV.read program (pos + 1) >>= MV.read program else MV.read program (pos + 1)
  num2 <- if (code `mod` 10000) `div` 1000 == 0 then MV.read program (pos + 2) >>= MV.read program else MV.read program (pos + 2)
  targetPos <- MV.read program (pos + 3)
  MV.write program targetPos (op num1 num2)
  
inputOp :: Int -> Int -> Int -> MV.IOVector Int -> IO ()
inputOp input code pos program = do 
  targetPos <- MV.read program (pos + 1)
  MV.write program targetPos input
  
outputOp :: Int -> Int -> MV.IOVector Int -> IO (Int, Maybe Int)
outputOp code pos program = if (code `mod` 1000) `div` 100 == 0 
                            then MV.read program (pos + 1) >>= MV.read program >>= (\val -> return (pos + 2, Just val))
                            else MV.read program (pos + 1) >>= (\val -> return (pos + 2, Just val))

jumpOp :: (Int -> Bool) -> Int -> Int -> MV.IOVector Int -> IO Int 
jumpOp jumpCond code pos program = do 
  cond <- if (code `mod` 1000) `div` 100  == 0 then MV.read program (pos + 1) >>= MV.read program else MV.read program (pos + 1)
  if jumpCond cond 
  then if (code `mod` 10000) `div` 1000 == 0 then MV.read program (pos + 2) >>= MV.read program else MV.read program (pos + 2)
  else return (pos + 3)
      
compOp :: (Int -> Int -> Bool) -> Int -> Int -> MV.IOVector Int -> IO ()
compOp op = mathOp (\num1 num2 -> if num1 `op` num2 then 1 else 0)  

day7 :: IO Int
day7 = do 
  inputString <- readFile "day7.txt"
  let opcodes = map (read @Int) $ words [if c == ',' then ' ' else c | c <- inputString]
      perms = permutations [5,6,7,8,9]
  maximum <$> (forM perms $ \settings -> feedbackLoop opcodes settings)