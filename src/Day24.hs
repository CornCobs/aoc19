module Day24 where 

import qualified Data.Map.Strict as Map 
import Data.Map.Strict (Map, (!))
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Maybe
import Data.Function

day24 :: IO ()
day24 = do 
  let part1 = findRepeat $ parseGrid initialGrid
      part2 = Set.size $ stepN 200 $ parsePositions initialGrid
  putStrLn $ "Part 1: " ++ show part1 
  putStrLn $ "Part 2: " ++ show part2

data Tile = Bug | Ground deriving (Show, Eq)

initialGrid = 
  [ "..#.."
  , "##..#"
  , "##..."
  , "#####"
  , ".#.##"
  ]
  
example = 
  [ "....#"
  , "#..#."
  , "#..##"
  , "..#.."
  , "#...."
  ]
  
parseGrid :: [String] -> Map (Int, Int) Tile 
parseGrid rows = ifoldl (\grid (rowIx, row) -> ifoldl (\grid' (colIx, col) -> Map.insert (rowIx, colIx) (parseC col) grid') grid row) Map.empty rows
  where ifoldl f init ls = foldl f init $ zip [0..] ls
        parseC '.' = Ground 
        parseC '#' = Bug
        
showGrid :: Map (Int, Int) Tile -> [String]
showGrid grid = map (\row -> map (\col -> showTile (row,col)) [0..4]) [0..4]
  where showTile pos = case grid ! pos of 
          Bug -> '#'
          Ground -> '.'
        
getNeighbours :: (Int, Int) -> Map (Int, Int) Tile -> [Tile]
getNeighbours pos grid = mapMaybe (`Map.lookup` grid) $ neighbours pos
  where neighbours (row, col) = [(row-1,col),(row+1,col),(row,col-1),(row,col+1)]
  
-- number of bugs around a position
adjacentBugs :: (Int, Int) -> Map (Int, Int) Tile -> Int 
adjacentBugs pos grid = length $ filter (== Bug) $ getNeighbours pos grid
  
-- update the grid state after 1 minute
step :: Map (Int, Int) Tile -> Map (Int, Int) Tile
step grid = Map.mapWithKey updateTile grid 
  where updateTile pos tile = case tile of 
          Bug -> if numBugs == 1 then Bug else Ground 
          Ground -> if numBugs == 1 || numBugs == 2 then Bug else Ground
          where numBugs = adjacentBugs pos grid  
          
biodiversityRating :: Map (Int, Int) Tile -> Int 
biodiversityRating grid = Map.foldrWithKey (\(row,col) _ rating -> rating + 2 ^ (row*5 + col)) 0 $ Map.filter (== Bug) grid

findRepeat :: Map (Int, Int) Tile -> Int 
findRepeat grid = go Set.empty grid 
  where go prevRatings grid = let grid' = step grid 
                                  rating = biodiversityRating grid' 
                              in  if Set.member rating prevRatings
                                    then rating 
                                    else go (Set.insert rating prevRatings) grid'

-- Part 2
data Position = Position { level :: Int, position :: (Int, Int) } deriving (Eq, Ord, Show)

parsePositions :: [String] -> Set Position 
parsePositions = Set.map (\p -> Position 0 p) . Map.keysSet . Map.filter (== Bug) . parseGrid 

validPositions :: Set (Int, Int)
validPositions = Set.cartesianProduct x x & Set.delete (2,2)
  where x = Set.fromDistinctAscList [0..4]

getNeighbourPositions :: Position -> Set Position 
getNeighbourPositions (Position lev (row,col)) =  (Set.map (\pos -> Position lev pos) $ Set.intersection validPositions neighbours)
                                               <> top <> bot <> left <> right 
                                               <> innerTop <> innerBot <> innerLeft <> innerRight
  where top = if row == 0 then Set.singleton $ Position (lev-1) (1,2) else Set.empty
        bot = if row == 4 then Set.singleton $ Position (lev-1) (3,2) else Set.empty
        left = if col == 0 then Set.singleton $ Position (lev-1) (2,1) else Set.empty
        right = if col == 4 then Set.singleton $ Position (lev-1) (2,3) else Set.empty
        innerTop = if (row,col) == (1,2) then Set.fromList $ map (\col' -> Position (lev+1) (0,col')) [0..4] else Set.empty
        innerBot = if (row,col) == (3,2) then Set.fromList $ map (\col' -> Position (lev+1) (4,col')) [0..4] else Set.empty
        innerLeft = if (row,col) == (2,1) then Set.fromList $ map (\row' -> Position (lev+1) (row',0)) [0..4] else Set.empty
        innerRight = if (row,col) == (2,3) then Set.fromList $ map (\row' -> Position (lev+1) (row',4)) [0..4] else Set.empty
        neighbours = Set.fromList [(row-1,col),(row+1,col),(row,col-1),(row,col+1)]
        
stepRecursive :: Set Position -> Set Position 
stepRecursive bugs = Set.filter updateTile allNeighbours
  where allNeighbours = Set.unions $ Set.map getNeighbourPositions bugs
        updateTile position = let isCurrentlyBug = Set.member position bugs 
                                  neighbourBugs = Set.size $ Set.intersection bugs $ getNeighbourPositions position 
                              in  if isCurrentlyBug
                                    then case neighbourBugs of 1 -> True 
                                                               _ -> False 
                                    else case neighbourBugs of 1 -> True 
                                                               2 -> True 
                                                               _ -> False
                                                               
stepN :: Int -> Set Position -> Set Position 
stepN 0 bugs = bugs 
stepN n bugs = let bugs' = stepRecursive bugs 
               in  stepN (n-1) bugs'