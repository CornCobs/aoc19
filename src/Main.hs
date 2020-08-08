{-# LANGUAGE TypeApplications, BangPatterns #-}

module Main where

import Data.List
import Data.Ord (comparing)
import Control.Monad.ST
import Control.Monad (guard, forM_)

import Data.Vector (fromList, unsafeThaw)
import qualified Data.Vector.Mutable as MV

import Data.Map.Strict ((!))
import qualified Data.Map.Strict as Map

import Control.Arrow ((&&&))
import Data.Function ((&))
import Data.Monoid

import qualified ST.IntCodeST as ST 
import qualified ST.STPrograms as ST

import qualified Day16

main = return ()

-- Day 1

day1 :: IO ()
day1 = do
   ans <- totalModuleFuel . map (read @Int) . lines <$> readFile "day1.txt"
   print ans

totalModuleFuel :: [Int] -> Int 
totalModuleFuel = sum . map totalFuelReq

totalFuelReq :: Int -> Int
totalFuelReq mod = sum (unfoldr getAdditionalFuel $ mod)

getAdditionalFuel :: Int -> Maybe (Int, Int)
getAdditionalFuel fuel
  | nextFuel < 0 = Nothing
  | otherwise    = Just (nextFuel, nextFuel)
  where nextFuel = fuel `div` 3 - 2

-- Day 2
day2 :: IO ()
day2 = do 
  inputString <- readFile "day2.txt"
  let opcodes = map (read @Int) $ words [if c == ',' then ' ' else c | c <- inputString]
  print (calculateParams opcodes)

calculateParams opcodes = do 
  noun <- [0..99]
  verb <- [0..99]
  guard $ intCode opcodes noun verb == 19690720
  return $ 100 * noun + verb

intCode :: [Int] -> Int -> Int -> Int
intCode opcodes noun verb = runST $ do
  program <- unsafeThaw (fromList opcodes)
  MV.write program 1 noun
  MV.write program 2 verb
  let process pos = do 
        code <- MV.read program pos 
        case code of 
          1 -> opcode (+) pos program >> process (pos + 4)
          2 -> opcode (*) pos program >> process (pos + 4)
          99 -> return ()
  process 0
  MV.read program 0

opcode :: (Int -> Int -> Int) -> Int -> MV.STVector s Int -> ST s ()
opcode op pos program = do 
  num1 <- MV.read program (pos + 1) >>= MV.read program
  num2 <- MV.read program (pos + 2) >>= MV.read program
  targetPos <- MV.read program (pos + 3)
  MV.write program targetPos (op num1 num2)

-- Day 3
data Direction = U | D | L | R deriving (Eq, Show, Read)

parseDirection :: String -> (Direction, Int)
parseDirection (dir:dist) = (read [dir], read dist)

generatePath :: [(Direction, Int)] -> [(Int,Int,Int)] -> [(Int,Int,Int)]
generatePath [] path = path
generatePath ((dir,dist):dirs) prev@((x,y,s):_) = 
  let newPath = case dir of 
        U -> [(x, y+steps, s+steps) | steps <- [dist,dist-1..1]]
        D -> [(x, y-steps, s+steps) | steps <- [dist,dist-1..1]]
        L -> [(x-steps, y, s+steps) | steps <- [dist,dist-1..1]]
        R -> [(x+steps, y, s+steps) | steps <- [dist,dist-1..1]]
  in generatePath dirs (newPath ++ prev)
  
intersections :: [(Int, Int, Int)] -> [(Int, Int, Int)] -> [Int]
intersections path1 path2 = 
  let l1 = sort path1 
      l2 = sort path2 
      merge [] _ res = res 
      merge _ [] res = res 
      merge (x@(x1,y1,s1):xs) (y@(x2,y2,s2):ys) res
        | (x1,y1) == (x2,y2) = merge xs ys ((s1+s2):res)
        | (x1,y1) >  (x2,y2) = merge (x:xs) ys res 
        | otherwise = merge xs (y:ys) res 
  in merge l1 l2 []

getShortestManhattan :: [(Int, Int)] -> Int
getShortestManhattan = minimum . map manhattan . filter (/= (0,0)) 
  where manhattan (x,y) = abs x + abs y  

day3 :: IO ()
day3 = do 
  let parseDirections input = words [if c == ',' then ' ' else c | c <- input]
  [input1, input2] <- (map (map parseDirection . parseDirections).lines) <$> readFile "day3.txt"
  print $ minimum $ filter (/= 0) $ intersections (generatePath input1 [(0,0,0)]) (generatePath input2 [(0,0,0)])

-- Day 4
isValidPass :: String -> Bool
isValidPass pass = (any ((== 2).length) $ group pass) && (sort pass == pass) -- part 1 was any ((> 1).length) $ group pass

countPasswords :: Int
countPasswords = length $ filter (isValidPass . show) [183564..657474]

-- Day 5
diagnostic :: [Int] -> IO ()
diagnostic opcodes = do
  program <- unsafeThaw (fromList opcodes)
  let process pos = do 
        code <- MV.read program pos 
        case code `mod` 100 of 
          1 -> mathOp (+) code pos program >> process (pos + 4)
          2 -> mathOp (*) code pos program >> process (pos + 4)
          3 -> inputOp    code pos program >> process (pos + 2)
          4 -> outputOp   code pos program >> process (pos + 2)
          5 -> jumpOp (/= 0) code pos program >>= process 
          6 -> jumpOp (== 0) code pos program >>= process 
          7 -> compOp (<)  code pos program >> process (pos + 4)
          8 -> compOp (==) code pos program >> process (pos + 4)
          99 -> return ()
  process 0

mathOp :: (Int -> Int -> Int) -> Int -> Int -> MV.IOVector Int -> IO ()
mathOp op code pos program = do 
  num1 <- if (code `mod` 1000)  `div` 100  == 0 then MV.read program (pos + 1) >>= MV.read program else MV.read program (pos + 1)
  num2 <- if (code `mod` 10000) `div` 1000 == 0 then MV.read program (pos + 2) >>= MV.read program else MV.read program (pos + 2)
  targetPos <- MV.read program (pos + 3)
  MV.write program targetPos (op num1 num2)
  
inputOp :: Int -> Int -> MV.IOVector Int -> IO ()
inputOp code pos program = do 
  input <- read <$> getLine
  targetPos <- MV.read program (pos + 1)
  MV.write program targetPos input
  
outputOp :: Int -> Int -> MV.IOVector Int -> IO ()
outputOp code pos program = if (code `mod` 1000) `div` 100 == 0 
                            then MV.read program (pos + 1) >>= MV.read program >>= print 
                            else MV.read program (pos + 1) >>= print

jumpOp :: (Int -> Bool) -> Int -> Int -> MV.IOVector Int -> IO Int 
jumpOp jumpCond code pos program = do 
  cond <- if (code `mod` 1000) `div` 100  == 0 then MV.read program (pos + 1) >>= MV.read program else MV.read program (pos + 1)
  if jumpCond cond 
  then if (code `mod` 10000) `div` 1000 == 0 then MV.read program (pos + 2) >>= MV.read program else MV.read program (pos + 2)
  else return (pos + 3)
      
compOp :: (Int -> Int -> Bool) -> Int -> Int -> MV.IOVector Int -> IO ()
compOp op = mathOp (\num1 num2 -> if num1 `op` num2 then 1 else 0)  

day5 :: IO ()
day5 = do 
  inputString <- readFile "day5.txt"
  let opcodes = map (read @Int) $ words [if c == ',' then ' ' else c | c <- inputString]
  diagnostic opcodes
  
-- Day 6
type Parent = String
type Child = String 

buildMap :: [String] -> Map.Map Child Parent
buildMap rels = foldr (\(parent, child) -> Map.insert child parent) Map.empty [(parent, child) | (parent, ')':child) <- map (break (== ')')) rels]

countOrbits :: Map.Map Child Parent -> Child -> Int
countOrbits _ "COM" = 0 
countOrbits relMap obj = 1 + countOrbits relMap (relMap ! obj)

parents :: Map.Map Child Parent -> Child -> [Parent]
parents _ "COM" = ["COM"]
parents relMap obj = obj : parents relMap (relMap ! obj)

minTransfers :: Map.Map Child Parent -> Int
minTransfers relMap = 
  let you = parents relMap "YOU"
      san = parents relMap "SAN"
      removeCommonAncestors ls1 ls2 = case (ls1, ls2) of 
        ([], _) -> (ls1, ls2)
        (_, []) -> (ls1, ls2) 
        (x:xs,y:ys)
          | x == y -> removeCommonAncestors xs ys 
          | otherwise -> (ls1, ls2)
      (path1, path2) = removeCommonAncestors (reverse you) (reverse san) 
  in length path1 + length path2 - 2

totalOrbits :: Map.Map Child Parent -> Int
totalOrbits relMap = Map.foldrWithKey (\child _ acc -> acc + countOrbits relMap child) 0 relMap

day6 :: IO ()
day6 = do 
  input <- readFile "day6.txt"
  print $ (minTransfers . buildMap . lines) input 

day7 :: IO ()
day7 = do 
  inputString <- readFile "day7.txt"
  let opcodes = map (read @Int) $ words [if c == ',' then ' ' else c | c <- inputString]
  diagnostic opcodes
  
-- Day 8
day8 :: IO ()
day8 = do 
  image <- (chunksOf 150.map (\c -> read @Int [c])) <$> readFile "day8.txt"
  -- Part 1
  let numZero = length . filter (==0)
      checkLayer = minimumBy (comparing numZero) image 
  print $ (length $ filter (==1) checkLayer) * (length $ filter (==2) checkLayer)
  -- Part 2
  let img = chunksOf 25 $ decode image
  forM_ img $ \row -> do 
    forM_ row $ \pixel -> putStr (if pixel == 1 then "0" else ".")
    putStrLn ""
  
chunksOf :: Int -> [a] -> [[a]]
chunksOf n [] = []
chunksOf n ls = let (chunk,rest) = splitAt n ls
                in chunk : chunksOf n rest
                
decode :: [[Int]] -> [Int]
decode img = foldl' (\topmost nextPixel -> if topmost == 2 then nextPixel else topmost) 2 <$> transpose img

-- Day 10

gradient (x0, y0) (x1, y1) = atan2 (fromIntegral (y1 - y0)) (fromIntegral (x1 - x0))

findBestLoc :: [(Int, Int)] -> (Int, Int)
findBestLoc asteroids = maximumBy (comparing numDetected) asteroids 
  where numDetected station = filter (/= station) asteroids 
                            & map (gradient station)
                            & sort 
                            & group 
                            & length

findMostAsteroids asteroids = maximum $ map numDetected asteroids 
  where numDetected station = filter (/= station) asteroids 
                            & map (gradient station)
                            & sort 
                            & group 
                            & length

parseAsteroids :: [[Char]] -> [(Int, Int)]
parseAsteroids rows = flip concatMap (zip [0..] rows) $ \(y, row) -> 
    flip concatMap (zip [0..] row) $ \(x, c) -> if c == '#' then [(x,y)] else []

getDestructionOrder :: [(Int, Int)] -> (Int, Int)
getDestructionOrder asteroids = 
  let station = findBestLoc asteroids 
      distX (x0, y0) (x1, y1) = (x0 - x1)^2 + (y0 - y1)^2
  in  filter (/= station) asteroids 
    & map (id &&& (angleFromUp station))
    & sortBy (comparing snd <> comparing (distX station . fst))
    & groupBy (\x y -> snd x == snd y)
    & transpose 
    & concat
    & (!! 199)
    & fst

angleFromUp :: (Int, Int) -> (Int, Int) -> Double
angleFromUp (x0, y0) (x1, y1)
  | baseAngle > pi/2 = (5 * pi/2) - baseAngle
  | otherwise        = (pi/2) - baseAngle 
  where baseAngle = atan2 (fromIntegral (y0-y1)) (fromIntegral (x1-x0)) 

day10 :: IO ()
day10 = do 
  input <- lines <$> readFile "day10.txt"
  --print $ findMostAsteroids $ parseAsteroids input
  print $ getDestructionOrder $ parseAsteroids input
  
-- Day 12
type Position = (Int, Int, Int) 
type Velocity = (Int, Int, Int)
vect op (x1,y1,z1) = (op x1,op y1,op z1)
vect2 op (x1,y1,z1) (x2,y2,z2) = (x1 `op` x2, y1 `op` y2, z1 `op` z2)

gravity (pos, initialVelocity) others = map (\other -> vect signum $ vect2 (-) other pos) others
                                      & foldr (vect2 (+)) initialVelocity
                    
step :: [(Position, Velocity)] -> [(Position, Velocity)]
step moons = let newVelocity = map (\moon -> gravity moon $ map fst $ filter (/= moon) moons) moons 
             in  zipWith (\(oldPos, _) velocity -> (vect2 (+) oldPos velocity, velocity)) moons newVelocity
             
totalEnergy (pos, vel) = sumCoords pos * sumCoords vel
  where sumCoords (x,y,z) = abs x + abs y + abs z
  
day12 = sum $ map totalEnergy $ stepN 1000 $ zip [(0,4,0),(-10,-6,-14),(9,-16,-3),(6,-1,2)] (repeat (0,0,0))
  
stepN 0 state = state
stepN x state = stepN (x-1) (step state)
        
day12pt2 = let initialState = zip [(0,4,0),(-10,-6,-14),(9,-16,-3),(6,-1,2)] (repeat (0,0,0))
               period fn !steps state = if fn state == fn initialState
                                        then steps
                                        else period fn (steps+1) (step state)
               periodX = period (map (\((px,_,_),(vx,_,_)) -> (px,vx))) 1 $ step initialState
               periodY = period (map (\((_,py,_),(_,vy,_)) -> (py,vy))) 1 $ step initialState
               periodZ = period (map (\((_,_,pz),(_,_,vz)) -> (pz,vz))) 1 $ step initialState
               lcm a b = a * b `div` gcd a b 
           in lcm periodX $ lcm periodY periodZ
            
           
example = zip [(-1,0,2), (2,-10,-7), (4,-8,8), (3,5,-1)] (repeat (0,0,0))
example2 = zip [(-8,-10,0), (5,5,10), (2,-7,3), (9,-8,-3)] (repeat (0,0,0))

  