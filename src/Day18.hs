{-# LANGUAGE DuplicateRecordFields #-}

module Day18 where

import qualified Data.Map.Strict as Map 
import Data.Map.Strict (Map, (!))
import Data.Char
import Data.List
import Data.Maybe
import Data.Function
import Control.Arrow
import Control.Monad
import Control.Monad.Trans.State
import qualified Data.Set as Set 

data Tile = Floor | Door Char | Key Char | You deriving (Eq, Show)
data Landmark = Landmark { tile :: Tile, distance :: Int } deriving (Eq, Show)
type TileNeighbours = (Tile, Map (Int, Int) Landmark)
data MazeState = MazeState { currentPosition :: (Int, Int), keysTaken :: Set.Set Char } deriving (Eq, Show, Ord)
type Cache s a = State (Map s Int) a 

instance Ord Landmark where 
  Landmark _ d1 `compare` Landmark _ d2 = d1 `compare` d2

-- First parse textual maze into workable format (map from positions to Tile data which contains necessary information about that position)
parseMaze :: [Char] -> ((Int, Int), Map (Int, Int) Tile) 
parseMaze = ifoldl parseLine ((-1,-1), Map.empty) . lines
  where ifoldl f init ls = foldl' f init $ zip [0..] ls
        parseLine currState (row, line) = 
          ifoldl (\(startPos, currMap) (col, tile) ->
                    case tile of '#' -> (startPos, currMap)
                                 '.' -> (startPos, Map.insert (row,col) Floor currMap)
                                 '@' -> ((row,col), Map.insert (row,col) You currMap)
                                 letter | isUpper letter -> (startPos, Map.insert (row,col) (Door $ toLower letter) currMap)
                                        | otherwise      -> (startPos, Map.insert (row,col) (Key letter) currMap)
                 ) currState line
                 
showMaze :: ((Int, Int), Map (Int, Int) Tile) -> [Char]
showMaze (startPos, maze) = concat $ flip map [0..80] $ \row -> 
                  flip map [0..80] (\col ->
                    case Map.lookup (row,col) maze of 
                      Nothing -> '#'
                      Just Floor -> '.'
                      Just (Door x) -> toUpper x 
                      Just (Key x) -> x
                      Just You -> '@') ++ "\n"

neighbours :: (Int, Int) -> [(Int, Int)]
neighbours (row,col) = [(row-1,col),(row+1,col),(row,col-1),(row,col+1)]

-- Next we build up the graph of connections between each door/key and other door/keys, as well as from the starting point
-- Each position now maps to a map of the next "key points" it can reach (keys/doors)
doorkeyGraph :: Map (Int, Int) Tile -> Map (Int, Int) TileNeighbours
doorkeyGraph maze = 
  let isImportant Floor = False 
      isImportant _ = True 
      keypts = Map.filter isImportant maze 
  in Map.mapWithKey (bfs maze) keypts
      
isReachable :: (Int, Int) -> Map (Int, Int) Landmark -> Bool
isReachable pos reachables = any (\neighbour -> case Map.lookup neighbour reachables of 
                                                        Nothing -> False
                                                        Just (Landmark Floor _) -> True 
                                                        Just (Landmark You _) -> True
                                                        Just _ -> False
                                 ) $ neighbours pos 
           
bfs :: Map (Int, Int) Tile -> (Int, Int) -> Tile -> TileNeighbours
bfs maze pos t = let (init', maze') = Map.partitionWithKey (\p t -> p `elem` neighbours pos) maze 
                     init = Map.map (\t -> Landmark t 1) init'
                     go steps explored unexplored = let (newlyExplored, stillUnexplored) = Map.partitionWithKey (\pos _ -> pos `isReachable` explored) unexplored
                                                    in  if null newlyExplored then explored 
                                                        else go (steps+1) (Map.union explored $ Map.map (\t -> Landmark t steps) newlyExplored) stillUnexplored
                 in go 2 init maze' 
                  & Map.filter (\(Landmark x _) -> x /= Floor && x /= You) 
                  & Map.delete pos 
                  & (\neighbourMap -> (t, neighbourMap))

{- Lastly, our maze path finding algorithm is as such:
  1. If no more keys in maze, we are done - report the steps needed, which is 0 
  2. If not, from current position, get list of keys that are reachable from here (filter snd of TileNeighbours for Key)
  3. Recursively call the algorithm on each possible branch with the new map gotten from takeKey, adding the distance to that key to the recursive call 
  4. Take the minimum of all these branches
  
  takeKey should work as such:
  1. If change position of You to the keyPos (and remove the You node)
  2. Remove the door 
  3. IF Door x and Key x are reachable from one to the other, 
     each of Door x's neighbours has its tileneighbours map updated to include doors neighbours and keys neighbours
  4. ELSE Door x's neighbours tileneighbours map updates to include door neighbours, 
     Key x's neighbours tileneighbours map updates to include key neighbours
-}

-- Attempt No. 1: Simply recursion over all possibilities without caching results. Takes way too long on real input (26 keys)
findOptimalRoute :: (Int, Int) -> Map (Int, Int) TileNeighbours -> Int 
findOptimalRoute currPos maze
  | Map.null availableKeys = 0 
  | otherwise = minimum $ map (\(keyPos, Landmark (Key char) dist) -> dist + findOptimalRoute keyPos (takeKey currPos keyPos char maze)) 
                        $ Map.toList availableKeys
  where availableKeys = reachableKeys maze currPos
        
reachableKeys :: Map (Int, Int) TileNeighbours -> (Int, Int) -> Map (Int, Int) Landmark 
reachableKeys maze currPos = Map.filter isKey (snd $ maze ! currPos)
  where isKey (Landmark (Key _) _) = True 
        isKey _ = False

-- The bulk of the work. Updates the maze graph after moving from currPos to keyPos, removing old position, the key one is on and the door it unlocks
-- (if any) and connecting newly connectable areas by adding door/keys neighbours to its neighbours neighbourlists 
takeKey :: (Int, Int) -> (Int, Int) -> Char -> Map (Int, Int) TileNeighbours -> Map (Int, Int) TileNeighbours
takeKey currPos keyPos char graph
    | null thedoor = Map.delete currPos graph 
                   & Map.adjust (first $ const You) keyPos 
                   & (\graph' -> foldr (updateGraph keyPos keyConnections) graph' (Map.toList keyConnections))
                   & Map.map (second (Map.delete keyPos))
    | otherwise = let (doorPos, (_, doorConnections)) = head $ thedoor
                      doorKeyConnected = Map.member keyPos doorConnections
                  in  Map.delete currPos graph 
                    & Map.adjust (first $ const You) keyPos
                    & Map.delete doorPos
                    & (if doorKeyConnected 
                       then let doorKeyDist = distance $ doorConnections ! keyPos 
                                keyConnectionsFromDoor = Map.delete doorPos keyConnections 
                                                       & Map.map (\(Landmark t' dist') -> (Landmark t' (dist' + doorKeyDist)))
                                doorConnectionsFromKey = Map.delete keyPos doorConnections
                                                       & Map.map (\(Landmark t' dist') -> (Landmark t' (dist' + doorKeyDist)))
                            in  (\graph' -> foldr (updateGraph doorPos (Map.unionWith min doorConnections keyConnectionsFromDoor)) graph' (Map.toList doorConnections))
                              . (\graph' -> foldr (updateGraph keyPos (Map.unionWith min keyConnections doorConnectionsFromKey)) graph' (Map.toList keyConnections))
                       else (\graph' -> foldr (updateGraph doorPos doorConnections) graph' (Map.toList doorConnections))
                          . (\graph' -> foldr (updateGraph keyPos keyConnections) graph' (Map.toList keyConnections)))
                    & Map.map (second (Map.delete keyPos . Map.delete doorPos))
    where thedoor = Map.toList $ Map.filter ((== Door char).fst) graph
          (_, keyConnections) = graph ! keyPos
          updateGraph :: (Int, Int) -> Map (Int, Int) Landmark -> ((Int, Int), Landmark) -> Map (Int, Int) TileNeighbours -> Map (Int, Int) TileNeighbours
          updateGraph pos connections (neighbourPos, Landmark t dist) graph' = Map.adjust (second $ \neighbours' -> 
                                                                                        Map.delete neighbourPos connections
                                                                                      & Map.map (\(Landmark t' dist') -> (Landmark t' (dist' + dist)))
                                                                                      & Map.unionWith min (Map.delete pos neighbours')
                                                                                      ) neighbourPos graph' 
        
findOptimalRouteCache :: MazeState -> Map (Int, Int) TileNeighbours -> Cache MazeState Int 
findOptimalRouteCache currState@(MazeState currPos keys) maze
  | Map.null availableKeys = return 0 
  | otherwise = do 
      alreadyCalculated <- inCache currState 
      case alreadyCalculated of 
        Just optimal -> return optimal 
        Nothing -> do 
          possibilities <- forM (Map.toList availableKeys) 
                             $ \(keyPos, Landmark (Key char) dist) -> do 
                                  optimal <- findOptimalRouteCache (MazeState keyPos $ Set.insert char keys) 
                                                                   (takeKey currPos keyPos char maze)
                                  return $ optimal + dist
          let best = minimum possibilities 
          modify' (Map.insert currState best)
          return best

  where availableKeys = reachableKeys maze currPos
        inCache currState = Map.lookup currState <$> get
        
test :: IO (Map (Int, Int) TileNeighbours,Map (Int, Int) TileNeighbours)
test = do 
  input <- readFile "day18test.txt"
  let (startPos, maze) = parseMaze input 
      g = doorkeyGraph maze 
      g' = takeKey (1,15) (1,17) 'a' g 
  return (g, g')
  
day18 :: IO ()
day18 = do 
  input <- readFile "day18.txt"
  let (startPos, maze) = parseMaze input 
      graph = doorkeyGraph maze 
  print graph 
  print $ evalState (findOptimalRouteCache (MazeState startPos Set.empty) graph) Map.empty
  
-- Part 2
-- for more optimal caching currentPositions should be a Set of 4 positions not a list
data MultiMaze = MultiMaze { currentPositions :: [(Int, Int)], keysTaken :: Set.Set Char } deriving (Eq, Show, Ord)
  
modifyMaze :: ((Int, Int), Map (Int, Int) Tile) -> ([(Int, Int)], Map (Int, Int) Tile) 
modifyMaze (start@(row,col), maze) = 
  let newStartPos = [(row+y,col+x) | y <- [-1,1], x <- [-1,1]]
      maze' = Map.delete start maze & (\maze'' -> foldr Map.delete maze'' $ neighbours start) 
                                    & (\maze'' -> foldr (\pos acc -> Map.insert pos You acc) maze'' $ newStartPos)
  in (newStartPos, maze')
  
        
findOptimalMultiRouteCache :: MultiMaze -> Map (Int, Int) TileNeighbours -> Cache MultiMaze Int 
findOptimalMultiRouteCache currState@(MultiMaze currPos keys) maze
  | null availableKeys = return 0 
  | otherwise = do 
      alreadyCalculated <- inCache currState 
      case alreadyCalculated of 
        Just optimal -> return optimal 
        Nothing -> do 
          possibilities <- forM availableKeys
                             $ \(pos, (keyPos, Landmark (Key char) dist)) -> do 
                                  let newState = MultiMaze (keyPos : filter (/= pos) currPos) $ Set.insert char keys
                                  optimal <- findOptimalMultiRouteCache newState (takeKey' pos keyPos char maze)
                                  return $ optimal + dist
          let best = minimum possibilities 
          modify' (Map.insert currState best)
          return best
  where availableKeys = concat $ map (\pos -> zip (repeat pos) $ Map.toList $ reachableKeys maze pos) currPos
        inCache currState = Map.lookup currState <$> get

-- In part 2 we have to modify takeKey and bfs to keep track of each You because another You's actions may cause updates in the You's neighbours      
doorkeyGraph' :: Map (Int, Int) Tile -> Map (Int, Int) TileNeighbours
doorkeyGraph' maze = 
  let isImportant Floor = False 
      isImportant _ = True 
      keypts = Map.filter isImportant maze 
  in Map.mapWithKey (bfs' maze) keypts
   
takeKey' :: (Int, Int) -> (Int, Int) -> Char -> Map (Int, Int) TileNeighbours -> Map (Int, Int) TileNeighbours
takeKey' currPos keyPos char maze = case Map.toList $ Map.filter ((== Door char).fst) maze of 
  [] -> Map.delete currPos maze 
      & Map.adjust (first $ const You) keyPos 
      & flip (foldr (updateMaze keyPos keyConnections)) (Map.toList keyConnections)
      & Map.map (second ( Map.delete currPos
                        . Map.adjust (\x -> x { tile = You }) keyPos))
  [(doorPos, (_, doorConnections))] -> 
    let doorKeyConnected = Map.member keyPos doorConnections
    in Map.delete currPos maze 
     & Map.adjust (first $ const You) keyPos 
     & Map.delete doorPos 
     & (if doorKeyConnected 
          then let doorKeyDist = distance $ doorConnections ! keyPos 
                   keyConnectionsFromDoor = Map.delete doorPos keyConnections 
                                          & Map.map (\(Landmark t' dist') -> (Landmark t' (dist' + doorKeyDist)))
                   doorConnectionsFromKey = Map.delete keyPos doorConnections
                                          & Map.map (\(Landmark t' dist') -> (Landmark t' (dist' + doorKeyDist)))
               in  flip (foldr (updateMaze doorPos (Map.unionWith min doorConnections keyConnectionsFromDoor))) (Map.toList doorConnections)
                 . flip (foldr (updateMaze keyPos (Map.unionWith min keyConnections doorConnectionsFromKey))) (Map.toList keyConnections)
          else flip (foldr (updateMaze doorPos doorConnections)) (Map.toList doorConnections)
             . flip (foldr (updateMaze keyPos keyConnections)) (Map.toList keyConnections))
     & Map.map (second ( Map.delete currPos 
                       . Map.delete doorPos
                       . Map.adjust (\x -> x { tile = You }) keyPos))

  where (_, keyConnections) = maze ! keyPos
        updateMaze :: (Int, Int) -> Map (Int, Int) Landmark -> ((Int, Int), Landmark) -> Map (Int, Int) TileNeighbours -> Map (Int, Int) TileNeighbours
        updateMaze pos connections (neighbourPos, Landmark t dist) maze' = 
          Map.adjust (second $ \neighboursNeighbours -> 
                       Map.unionWith min (Map.map (\x -> x { distance = dist + distance x}) connections)
                                         neighboursNeighbours
                     ) neighbourPos maze'

  
bfs' :: Map (Int, Int) Tile -> (Int, Int) -> Tile -> TileNeighbours
bfs' maze pos t = let (init', maze') = Map.partitionWithKey (\p t -> p `elem` neighbours pos) maze 
                      init = Map.map (\t -> Landmark t 1) init'
                      go steps explored unexplored = let (newlyExplored, stillUnexplored) = Map.partitionWithKey (\pos _ -> pos `isReachable` explored) unexplored
                                                     in  if null newlyExplored then explored 
                                                         else go (steps+1) (Map.union explored $ Map.map (\t -> Landmark t steps) newlyExplored) stillUnexplored
                  in go 2 init maze' 
                   & Map.filter (\(Landmark x _) -> x /= Floor) 
                   & Map.delete pos 
                   & (\neighbourMap -> (t, neighbourMap))