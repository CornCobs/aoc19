{-# LANGUAGE TypeApplications, RecursiveDo #-}

module ST.STPrograms where

import ST.IntCodeST

import Data.List 
import Data.Maybe (fromMaybe)
import qualified Data.Map.Strict as Map

import Control.Monad
import Control.Arrow (second)
import System.Console.ANSI
import Control.Concurrent (threadDelay)


-- Util
chunksOf :: Int -> [a] -> [[a]]
chunksOf n [] = []
chunksOf n ls = let (chunk,rest) = splitAt n ls
                in chunk : chunksOf n rest

-- day 7
day7 :: IO Integer
day7 = do 
  inputString <- readFile "day7.txt"
  let opcodes = map (read @Integer) $ words [if c == ',' then ' ' else c | c <- inputString]
      perms = permutations [5,6,7,8,9]
  return $ maximum $ map (feedbackLoop opcodes) perms

feedbackLoop :: [Integer] -> [Integer] -> Integer 
feedbackLoop opcodes [a,b,c,d,e] = 
  let ampA = intcode opcodes (a : 0 : ampE)
      ampB = intcode opcodes (b : ampA)
      ampC = intcode opcodes (c : ampB)
      ampD = intcode opcodes (d : ampC)
      ampE = intcode opcodes (e : ampD)
  in last ampE

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

-- Solution 1: Pure solving of the image (robot) followed by effectful printing out 
robot :: [Integer] -> Integer -> Map.Map (Int, Int) Integer
robot opcodes startingPanel =
  let output = intcode opcodes (startingPanel : input)
      ((_, _, panels), input) = mapAccumL nextInput ((0,0), Up, Map.singleton (0,0) startingPanel) $ chunksOf 2 output 
  in panels
        
nextInput (pos, facing, panels) [color, turn] = ((nextPos pos newfacing, newfacing, panels'), fromMaybe 0 (Map.lookup pos panels))
  where newfacing = if turn == 1 then succ facing else pred facing 
        panels'   = Map.insert pos color panels 
                
day11 :: IO ()
day11 = do 
  input <- readFile "day11.txt"
  let program = map (read @Integer) $ words [if c == ',' then ' ' else c | c <- input]
      pix = Map.filter (==1) $ robot program 1
  let (xs, ys) = (unzip . Map.keys) pix
  forM_ [maximum ys, maximum ys - 1..minimum ys] $ \y -> do 
    forM_ [minimum xs..maximum xs] $ \x -> 
      if Map.member (x,y) pix then putStr "#" else putStr " "
    putStrLn ""
     
-- Solution 2: Interleaving effects - printing to console as robot paints with animation!
day11' :: [Integer] -> Integer -> IO ()
day11' opcodes startingPanel = mdo
  let output = intcode opcodes (startingPanel : input)
      
      icon Up = "^"
      icon Down = "v"
      icon Forward = ">"
      icon Backward = "<"
      
      moveRobot (_, _, panels) [] = return (panels, [])
      moveRobot (pos, facing, panels) ([color,turn]:rest) = do 
        threadDelay 200000
        clearScreen 
        setCursorPosition 0 0 
        let newfacing = if turn == 1 then succ facing else pred facing 
            panels' = Map.insert pos color panels 
            move = fromMaybe 0 (Map.lookup pos panels)
            pix = Map.filter (==1) panels 
            (xs, ys) = unzip $ Map.keys $ pix
        when (not $ null xs) $ do 
            forM_ [maximum ys, maximum ys - 1..minimum ys] $ \y -> do 
              forM_ [minimum xs..maximum xs] $ \x -> do 
                let sprite | (x,y) == pos = icon facing 
                           | Map.member (x,y) pix = "#"
                           | otherwise = " "
                putStr sprite 
              putStrLn ""
        second (move:) <$> moveRobot (nextPos pos newfacing, newfacing, panels') rest
        
  (_, input) <- moveRobot ((0,0), Up, Map.singleton (0,0) startingPanel) $ chunksOf 2 output 
  return ()

-- day 13
data GameState = GameState { gameStart :: Bool, ball :: Integer, paddle :: Integer, score :: Integer } deriving Show

joystick (GameState gameStart ball paddle score) [x,y,tid] = ((GameState gameStart' ball' paddle' score'), move)
  where move | not gameStart = -2
             | ball < paddle = -1
             | ball > paddle = 1 
             | otherwise = 0
        gameStart' = gameStart || x == -1 
        ball'   = if tid == 4 then x else ball 
        paddle' = if tid == 3 then x else paddle
        score'  = if x == -1 then tid else score

move _ [] = return []
move gameState (gameObjects:rest) = do 
  let (gameState', action) = joystick gameState gameObjects
  --printGameObjects gameObjects
  print gameState'
  if gameStart gameState' 
    then (action :) <$> move gameState' rest
    else move gameState' rest

arcadeGame :: [Integer] -> IO ()
arcadeGame opcodes = mdo 
  let output = intcode opcodes input          
      initialState = GameState False 0 0 0
      
  input <- move initialState $ chunksOf 3 output
  return ()

day13 :: IO ()
day13 = do 
  input <- readFile "day13.txt"
  let program = map (read @Integer) $ words [if c == ',' then ' ' else c | c <- input]
  clearScreen
  arcadeGame (2 : tail program)
  setCursorPosition 22 0 

-- ATTEMPT #?? - do not use IO (since currently facing fixIOException due to some strictness in move probably by the if condition)
-- Value recursion fails if function is strict in its input i.e. forces evaluation of output. Suspect that conditional adding of input 
-- forces evaluation in the `if gameStart gameState' ` part, thus not working 
-- Alternative proposal - add everything to input but filter out invalid input (i.e. while not gameStart)

arcadeText :: [Integer] -> Integer 
arcadeText opcodes = 
  let output = intcode opcodes $ filter (/= (-2)) input 
      move gameState [] = (score gameState, [])
      move gameState (gameObjects:rest) = let (gameState', action) = joystick gameState gameObjects 
                                          in  second (action :) $ move gameState' rest 
                                              
      (finalScore, input) = move (GameState False 0 0 0) $ chunksOf 3 output
  in finalScore

printGameObjects [x,y,tid] = 
  if x == -1 
    then setCursorPosition 0 0 >> putStr ("Score: " ++ show tid)
    else setCursorPosition (fromIntegral y + 1) (fromIntegral x) >> putStr (icon tid)
  where icon 0 = " "
        icon 1 = "#"
        icon 2 = "*"
        icon 3 = "="
        icon 4 = "o"