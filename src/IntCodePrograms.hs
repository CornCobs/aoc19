{-# LANGUAGE TypeApplications, RecursiveDo #-}

module IntCodePrograms where

import IntCode 

import Data.List 
import Data.Maybe (fromMaybe)
import qualified Data.Map.Strict as Map

import Control.Monad
import System.Console.ANSI

-- day 7  
day7 :: IO Integer
day7 = do 
  inputString <- readFile "day7.txt"
  let opcodes = map (read @Integer) $ words [if c == ',' then ' ' else c | c <- inputString]
      perms = permutations [5,6,7,8,9]
  maximum <$> mapM (\settings -> loop opcodes settings) perms
  
simpleLoop :: [Integer] -> IO [Integer]
simpleLoop opcodes = mdo 
  output <- intcode opcodes (4:signal)
  let signal = reverse output 
  return $ take 10 signal 
  
simpleLoop' :: [Integer] -> IO [Integer]
simpleLoop' opcodes = mdo 
  output <- intcode opcodes (4:reverse output)
  return $ take 10 output

loop :: [Integer] -> [Integer] -> IO Integer 
loop opcodes [a,b,c,d,e] = mdo 
  ampA <- intcode opcodes (a : 0 : reverse ampE)
  ampB <- intcode opcodes (b : reverse ampA)
  ampC <- intcode opcodes (c : reverse ampB)
  ampD <- intcode opcodes (d : reverse ampC)
  ampE <- intcode opcodes (e : reverse ampD)
  return $ head ampE

-- day 11
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

robot :: [Integer] -> Integer -> IO (Map.Map (Int, Int) Integer)
robot opcodes startingPanel = mdo 
  output <- intcode opcodes (startingPanel : reverse inputReversed)
  let ((_, _, panels), inputReversed) = mapAccumR nextInput ((0,0), Up, Map.singleton (0,0) startingPanel) $ chunksOf 2 output 
  return panels 
        
nextInput (pos, facing, panels) [turn, color] = ((nextPos pos newfacing, newfacing, panels'), fromMaybe 0 (Map.lookup pos panels))
  where newfacing = if turn == 1 then succ facing else pred facing 
        panels'   = Map.insert pos color panels 
        
chunksOf :: Int -> [a] -> [[a]]
chunksOf n [] = []
chunksOf n ls = let (chunk,rest) = splitAt n ls
                in chunk : chunksOf n rest
                
day11 :: IO ()
day11 = do 
  input <- readFile "day11.txt"
  let program = map (read @Integer) $ words [if c == ',' then ' ' else c | c <- input]
  pix <- Map.filter (==1) <$> robot program 1
  let (xs, ys) = (unzip . Map.keys) pix
  forM_ [maximum ys, maximum ys - 1..minimum ys] $ \y -> do 
    forM_ [minimum xs..maximum xs] $ \x -> 
      if Map.member (x,y) pix then putStr "#" else putStr " "
    putStrLn ""
    
-- day 13 
day13 :: IO ()
day13 = do 
  input <- readFile "day13.txt"
  let program = map (read @Integer) $ words [if c == ',' then ' ' else c | c <- input]
  clearScreen
  arcadeGame (program)
  setCursorPosition 22 0 

arcadeGame :: [Integer] -> IO ()
arcadeGame opcodes = mdo 
  output <- intcode opcodes (0 : reverse input)
  let gameObjects = chunksOf 3 $ reverse output
      ((_,_,score), input) = mapAccumR joystick (0,0,0) $ chunksOf 3 $ output 
      icon 0 = " "
      icon 1 = "#"
      icon 2 = "*"
      icon 3 = "="
      icon 4 = "o"
  forM_ gameObjects $ \[x,y,tid] -> do 
    if x == -1 
      then setCursorPosition 0 0 >> putStr ("Score: " ++ show tid)
      else setCursorPosition (fromIntegral y + 1) (fromIntegral x) >> putStr (icon tid)
      
joystick (ball, paddle, score) [x,y,tid] = ((ball', paddle', score'), move)
  where move | ball < paddle = -1
             | ball > paddle = 1 
             | otherwise = 0
        ball'   = if tid == 4 then x else ball 
        paddle' = if tid == 3 then x else paddle
        score'  = if x == -1 then tid else score
  
data GameState = GameState { gameStart :: Bool, ball :: Integer, paddle :: Integer, score :: Integer }  

joystick' (GameState gameStart ball paddle score) [x,y,tid] = ((GameState gameStart' ball' paddle' score'), move)
  where move | ball < paddle = -1
             | ball > paddle = 1 
             | otherwise = 0
        gameStart' = gameStart || x == -1 
        ball'   = if tid == 4 then x else ball 
        paddle' = if tid == 3 then x else paddle
        score'  = if x == -1 then tid else score
  
arcadeGame' opcodes = mdo 
  output <- intcode opcodes (0 : input)
  let gameObjects = chunksOf 3 $ reverse output
      initialState = GameState False 0 0 0
  input <- gameMove initialState gameObjects
  return ()

gameMove _ [] = return []
gameMove gameState (gameObjects:rest) = do 
  let (gameState', move) = joystick' gameState gameObjects
  printGameObjects gameObjects
  if gameStart gameState' 
    then (move :) <$> gameMove gameState' rest 
    else gameMove gameState' rest
    
printGameObjects [x,y,tid] = 
  if x == -1 
    then setCursorPosition 0 0 >> putStr ("Score: " ++ show tid)
    else setCursorPosition (fromIntegral y + 1) (fromIntegral x) >> putStr (icon tid)
  where icon 0 = " "
        icon 1 = "#"
        icon 2 = "*"
        icon 3 = "="
        icon 4 = "o"