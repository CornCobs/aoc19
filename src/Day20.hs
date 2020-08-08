module Day20 where

import qualified Data.Map.Strict as Map 
import           Data.Map.Strict        ((!))
import qualified Data.Set        as Set 
import           Data.Set               ((\\))
import           Data.List hiding       ((\\))
import           Data.Char
import           Data.Maybe
import           Data.Function

data Edge = Inner | Outer deriving (Eq, Show)
data Warp = Warp deriving (Eq, Show)
data Tile a = Floor | Portal a String deriving (Eq, Show)
type WarpTile = Tile Warp
type RecursiveTile = Tile Edge


portalCode :: Tile a -> String
portalCode Floor = ""
portalCode (Portal _ x) = x

class PortalType a where
  makePortal :: [[Char]] -> ((Int, Int), Char) -> ((Int, Int), Char) -> Tile a 
  
instance PortalType Warp where 
  makePortal _ ((row1, col1), letter1) ((row2, col2), letter2) = 
    if row1 == row2 then if col1 < col2 
                         then Portal Warp [letter1, letter2]
                         else Portal Warp [letter2, letter1]
                    else if row1 > row2 
                         then Portal Warp [letter2, letter1]
                         else Portal Warp [letter1, letter2]

instance PortalType Edge where 
  makePortal rows ((row1, col1), letter1) ((row2, col2), letter2) = 
    if row1 == row2 then if col1 < col2 
                         then Portal edge [letter1, letter2]
                         else Portal edge [letter2, letter1]
                    else if row1 > row2 
                         then Portal edge [letter2, letter1]
                         else Portal edge [letter1, letter2]
    where edge | row2 == 0 || row2 == (length rows - 1) || col2 == 0 || col2 == (length (head rows) - 1) = Outer 
               | otherwise = Inner

day20 :: IO ()
day20 = do 
  input <- readFile "day20.txt"
  print $ bfs $ parseMaze input

parseMaze :: PortalType a => [Char] -> Map.Map (Int, Int) (Tile a)
parseMaze input = ifoldl (\maze line row -> ifoldl (parseCell row) maze line) Map.empty rows 
  where rows = lines input 
        ifoldl f init ls = foldl' (\acc (ix, li) -> f acc li ix) init $ zip [0..] ls
        parseCell row maze cell col = case cell of 
          '.' -> Map.insert (row,col) (parsePortal rows (row,col)) maze 
          _   -> maze 

        
neighbours :: (Int, Int) -> [(Int, Int)]
neighbours (row,col) = [(row-1,col),(row+1,col),(row,col-1),(row,col+1)]

parsePortal :: PortalType a => [[Char]] -> (Int, Int) -> Tile a 
parsePortal rows pos = fromMaybe Floor $ do 
  let get (row,col) = ((row,col), rows !! row !! col)
      findAdjacentLetter = listToMaybe . filter (isAlpha.snd) . map get . neighbours
  letter1 <- findAdjacentLetter pos 
  letter2 <- findAdjacentLetter (fst letter1)
  return $ makePortal rows letter1 letter2
      
mapToNeighbours :: Map.Map (Int, Int) WarpTile -> Map.Map (Int, Int) (Set.Set (Int, Int))
mapToNeighbours maze = Map.mapWithKey (\pos tile -> Set.fromList $ filter (flip Map.member maze) (neighbours pos) 
                                                 ++ case tile of Floor -> []
                                                                 x -> Map.keys $ Map.filter ((== portalCode x).portalCode) $ Map.delete pos maze
                                      ) maze
                                      
bfs :: Map.Map (Int, Int) WarpTile -> Int 
bfs maze = go 0 (Set.singleton startPos)
  where neighbourMap = mapToNeighbours maze 
        startPos = head $ Map.keys $ Map.filter ((== "AA").portalCode) maze 
        endPos = head $ Map.keys $ Map.filter ((== "ZZ").portalCode) maze 
        go :: Int -> Set.Set (Int,Int) -> Int
        go steps covered
          | Set.member endPos covered = steps 
          | otherwise = go (steps+1) $ Map.foldl' Set.union covered $ Map.restrictKeys neighbourMap covered
          
-- part 2 
data FullPosition = FullPosition { level :: Int, position :: (Int, Int) } deriving (Eq, Show, Ord)
data Tile2 = Floor2 | Goal | Start | Connection Edge (Int, Int) deriving (Eq, Show)

tile2Maze maze = Map.mapWithKey 
                 (\pos tile -> case tile of 
                     Floor -> Floor2 
                     Portal _ "AA" -> Start 
                     Portal _ "ZZ" -> Goal
                     Portal Inner x -> Connection Inner $ head $ Map.keys $ Map.filter (== Portal Outer x) maze
                     Portal Outer x -> Connection Outer $ head $ Map.keys $ Map.filter (== Portal Inner x) maze
                 ) maze 

getNeighbours :: FullPosition -> Map.Map (Int, Int) Tile2 -> Set.Set FullPosition 
getNeighbours (FullPosition lev pos) maze = Set.fromList $ extraTileIfPortal ++ (map (\p -> FullPosition lev p) $ filter (flip Map.member maze) (neighbours pos)) 
  where extraTileIfPortal = case maze ! pos of 
          Goal -> []
          Start -> []
          Floor2 -> []
          Connection Inner x -> [FullPosition (lev+1) x]
          Connection Outer x | lev == 0  -> []
                             | otherwise -> [FullPosition (lev-1) x]
                         
recursiveMazeSearch :: Map.Map (Int, Int) Tile2 -> Int 
recursiveMazeSearch maze = go 0 (Set.singleton startPos) (Set.singleton startPos) Set.empty
  where startPos = FullPosition 0 $ head $ Map.keys $ Map.filter (== Start) maze
        goalPos  = FullPosition 0 $ head $ Map.keys $ Map.filter (== Goal)  maze
        go steps covered edges prevEdges
          | Set.member goalPos covered = steps 
          | otherwise = go (steps+1) newCovered newEdges edges
            where newEdges = Set.unions (Set.map (`getNeighbours` maze) edges) \\ prevEdges
                  newCovered = Set.union covered newEdges
          
          