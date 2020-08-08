{-# LANGUAGE TypeApplications #-}

module Day11 where

import Day9 hiding (inputOp, outputOp)

import Data.List
import Control.Monad (forM_, liftM2, liftM3)

import Data.Vector (fromList, unsafeThaw)
import qualified Data.Vector.Mutable as MV
import Data.IORef

import Control.Monad.Trans.Maybe
import qualified Data.Map.Strict as Map 
import Data.Maybe (fromMaybe)

import System.Console.ANSI
import Control.Monad.Loops

intcode :: [Integer] -> IO (Integer -> MaybeT IO Integer)
intcode opcodes = do 
  program <- MV.replicate 10000 0
  forM_ (zip [0..] opcodes) $ \(ix, opcode) -> MV.write program ix opcode
  posref <- newIORef 0 :: IO (IORef Int)
  relbaseref <- newIORef 0 :: IO (IORef Integer)
  return $ \input -> MaybeT $ do
              let process relbase pos = do
                    code <- MV.read program pos
                    case code `mod` 100 of 
                      1 -> mathOp (+) relbase code pos program >> process relbase (pos + 4)
                      2 -> mathOp (*) relbase code pos program >> process relbase (pos + 4)
                      3 -> inputOp input relbase code pos program >> process relbase (pos + 2)
                      4 -> outputOp relbase code pos program
                      5 -> jumpOp (/= 0) relbase code pos program >>= process relbase 
                      6 -> jumpOp (== 0) relbase code pos program >>= process relbase
                      7 -> compOp (<)  relbase code pos program >> process relbase (pos + 4)
                      8 -> compOp (==) relbase code pos program >> process relbase (pos + 4)
                      9 -> relBaseOffsetOp relbase code pos program >>= (\newBase -> process newBase (pos + 2))
                      99 -> return (pos, relbase, Nothing)
              posval <- readIORef posref
              relbaseval <- readIORef relbaseref
              (nextPos, relbaseval', ret) <- process relbaseval posval
              writeIORef posref nextPos
              writeIORef relbaseref relbaseval'
              return ret
  
inputOp :: Integer -> Integer -> Integer -> Int -> MV.IOVector Integer -> IO ()
inputOp input relbase code pos program = do 
  targetPos <- if (code `mod` 1000) `div` 100 == 0 
               then fromIntegral <$> MV.read program (pos + 1)
               else (fromIntegral . (+relbase)) <$> MV.read program (pos + 1)
  MV.write program targetPos input
  
outputOp :: Integer -> Integer -> Int -> MV.IOVector Integer -> IO (Int, Integer, Maybe Integer)
outputOp relbase code pos program = getVal 1 relbase code pos program >>= (\val -> return (pos + 2, relbase, Just val))

data Facing = Up | Down | Forward | Backward deriving Show

instance Enum Facing where
  toEnum i = case i `mod` 4 of 
    0 -> Up
    1 -> Forward
    2 -> Down
    3 -> Backward
  fromEnum Up = 0
  fromEnum Forward = 1
  fromEnum Down = 2
  fromEnum Backward = 3

nextPos (x,y) Up = (x, y+1)
nextPos (x,y) Forward = (x+1, y)
nextPos (x,y) Down = (x, y-1)
nextPos (x,y) Backward = (x-1, y)

robot :: [Integer] -> IO (Map.Map (Int, Int) Integer)
robot opcodes = do 
  command <- intcode opcodes
  let run currentPos facing panels = do 
        let currPanel = fromMaybe 0 (Map.lookup currentPos panels) 
        output <- runMaybeT $ liftM2 (,) (command currPanel) (command currPanel)
        case output of 
          Nothing -> return panels
          Just (col, dir) -> let newfacing = if dir == 1 then succ facing else pred facing
                             in run (nextPos currentPos newfacing) 
                                    newfacing
                                    (Map.insert currentPos col panels)
  run (0,0) Up $ Map.singleton (0,0) 1

day11 :: IO ()
day11 = do 
  input <- readFile "day11.txt"
  let program = map (read @Integer) $ words [if c == ',' then ' ' else c | c <- input]
  pix <- Map.filter (==1) <$> robot program 
  let (xs, ys) = (unzip . Map.keys) pix
  forM_ [maximum ys, maximum ys - 1..minimum ys] $ \y -> do 
    forM_ [minimum xs..maximum xs] $ \x -> 
      if Map.member (x,y) pix then putStr "#" else putStr " "
    putStrLn ""
      
-- day 13
day13 :: IO ()
day13 = do 
  input <- readFile "day13.txt"
  let (_:program) = map (read @Integer) $ words [if c == ',' then ' ' else c | c <- input]
  game <- intcode (2:program)
  clearScreen
  ballpaddlepos <- newIORef (0,0) :: IO (IORef (Int, Int))
  whileJust_ (do 
    move <- controlJoystick <$> readIORef ballpaddlepos
    runMaybeT $ liftM3 (,,) (game move) (game move) (game move)
    ) $ \(x,y,tid) -> do 
      setCursorPosition (fromIntegral (y+1)) (fromIntegral x)
      case tid of 0 -> putStr " "
                  1 -> putStr "\x2588"
                  2 -> putStr "\x2593"
                  3 -> putStr "_" >> modifyIORef ballpaddlepos (\(ball, _) -> (ball, fromIntegral x))
                  4 -> putStr "o"      >> modifyIORef ballpaddlepos (\(_, paddle) -> (fromIntegral x, paddle))
                  score -> setCursorPosition 0 0 >> putStr ("Score: " ++ show score)
  setCursorPosition 22 0 
  
controlJoystick :: (Int, Int) -> Integer
controlJoystick (ball, paddle)
  | ball < paddle = -1
  | ball > paddle = 1 
  | otherwise = 0
