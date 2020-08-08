{-# LANGUAGE TypeApplications #-}

module Day15 where

import ST.IntCodeST
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map, (!))
import Control.Arrow (second)
import qualified Data.Set as Set
import Data.Set (Set, (\\), size)
import Data.Function
import Control.Monad (when, forM_)

import Debug.Trace
import System.Console.ANSI
import Control.Concurrent (threadDelay)

data Direction = North | South | West | East deriving (Eq, Ord, Enum, Show)
data MazeState = MazeState { position :: (Int, Int)
                           , explored :: Map (Int, Int) (Set Direction)
                           , steps    :: [Direction]
                           , lastMove :: Direction 
                           , backTracked :: Bool }
                           deriving Show 

allDirections = Set.fromList [North .. East]
nextPos (row,col) dir = case dir of 
  North -> (row-1,col)
  East  -> (row,col+1)
  South -> (row+1,col)
  West  -> (row,col-1)
  
nextMove exp pos lastMv
  | Map.notMember pos exp = Just $ head $ filter (/= opposite lastMv) [North, East, South, West]
  | Map.notMember (nextPos pos North) exp && Set.notMember North (exp ! pos) = Just North 
  | Map.notMember (nextPos pos East) exp && Set.notMember East (exp ! pos) = Just East
  | Map.notMember (nextPos pos South) exp && Set.notMember South (exp ! pos) = Just South
  | Map.notMember (nextPos pos West) exp && Set.notMember West (exp ! pos) = Just West 
  | otherwise = Nothing
  
  
findOxygen :: [Integer] -> MazeState 
findOxygen opcodes = finalState
  where output = intcode opcodes (1:input)
        dfs state@(MazeState pos exp st lastMv back) (x:xs) = case traceShowId x of 
          2 -> (state { steps = lastMv : st } , [])
          0 -> let (back', nextMv) = case traceShowId $ nextMove exp pos lastMv of 
                     Just dir -> (False, dir)
                     Nothing  -> (True, opposite $ head st)
                   exp' = Map.insertWith Set.union pos (Set.singleton nextMv) exp
               in traceShow pos $ second (toInteger (fromEnum nextMv) + 1 :) $ dfs (MazeState pos exp' st nextMv back') xs 
          1 -> let st' = if back then tail st else lastMv : st 
                   pos' = nextPos pos lastMv 
                   (back', nextMv) = case traceShowId $ nextMove exp pos' lastMv of 
                     Just dir -> (False, dir)
                     Nothing  -> (True, opposite $ head st')
                   exp' = Map.insertWith Set.union pos' (Set.singleton nextMv) exp
               in traceShow pos' $ second (toInteger (fromEnum nextMv) + 1 :) $ dfs (MazeState pos' exp' st' nextMv back') xs
                 
        (finalState, input) = dfs (MazeState (0,0) (Map.singleton (0,0) (Set.singleton North)) [] North False) output
    
completeMap :: [Integer] -> Map (Int, Int) Integer
completeMap opcodes = mazeMap
  where output = intcode opcodes (1:input)
        dfs state@(MazeState pos exp st lastMv back) mazeMap (x:xs) = case x of 
          0 -> let (back', nextMv) = case nextMove exp pos lastMv of 
                     Just dir -> (False, dir)
                     Nothing  -> (True, opposite $ head st)
                   exp' = Map.insertWith Set.union pos (Set.singleton nextMv) exp
               in traceShow pos $ second (toInteger (fromEnum nextMv) + 1 :) $ dfs (MazeState pos exp' st nextMv back') mazeMap xs 
          y -> let st' = if back then tail st else lastMv : st 
                   pos' = nextPos pos lastMv 
                   (back', nextMv) = case nextMove exp pos' lastMv of 
                     Just dir -> (False, dir)
                     Nothing  -> (True, opposite $ head st')
                   exp' = Map.insertWith Set.union pos' (Set.singleton nextMv) exp
                   mazeMap' = Map.insert pos' y mazeMap
               in if back' && null st'
                  then (mazeMap, [])
                  else traceShow pos' $ second (toInteger (fromEnum nextMv) + 1 :) $ dfs (MazeState pos' exp' st' nextMv back') mazeMap' xs
                 
        (mazeMap, input) = dfs (MazeState (0,0) (Map.singleton (0,0) (Set.singleton North)) [] North False) (Map.singleton (0,0) 1) output
    
printMaze :: Map (Int, Int) Integer -> IO ()
printMaze mazeMap = do 
  let (rows, cols) = unzip $ Map.keys mazeMap 
  forM_ [minimum rows..maximum rows] $ \row -> do 
    forM_ [minimum cols..maximum cols] $ \col ->
      case Map.lookup (row,col) mazeMap of 
        Nothing -> putStr "#"
        Just 1 -> putStr " "
        Just 2 -> putStr "O"
    putStrLn ""
    
fillOxygen :: Map (Int, Int) Integer -> Int 
fillOxygen mazeMap = go (Map.keysSet oxygen) (Map.keysSet rest) 0
  where (oxygen, rest) = Map.partition (== 2) mazeMap 
        go oxygenated unoxygenated minutes = 
          let (newlyOxygenated, unoxygenated') = Set.partition (\pos -> any (flip Set.member oxygenated) $ map (nextPos pos) [North .. East]) unoxygenated
          in  if Set.null unoxygenated then minutes else go (Set.union oxygenated newlyOxygenated) unoxygenated (minutes + 1)
        
fillOxygenIO :: Map (Int, Int) Integer -> IO ()
fillOxygenIO mazeMap = go mazeMap 0 
  where isNextToOxygen pos currMap = flip any (map (nextPos pos) [North .. East])
                                              (\neighbour -> case Map.lookup neighbour currMap of 
                                                               Just 2 -> True
                                                               _ -> False)
        go currMap minutes = do 
          threadDelay 300000
          clearScreen
          print minutes 
          printMaze currMap 
          if all (==2) currMap 
            then return ()
            else go (Map.mapWithKey (\pos val -> if val == 2 || isNextToOxygen pos currMap 
                                                   then 2 else 1) currMap) (minutes + 1)
        
opposite North = South 
opposite South = North
opposite East = West 
opposite West = East 

day15 :: IO [Integer]
day15 = do 
  input <- readFile "day15.txt"
  return $ map (read @Integer) $ words [if c == ',' then ' ' else c | c <- input]
  