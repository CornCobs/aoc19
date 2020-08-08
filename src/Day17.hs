module Day17 where

import ST.IntCodeST
import Data.Char (chr, ord)
import Control.Monad

import qualified Data.Set as Set
import Data.Set (Set)


input :: IO [Integer]
input = do 
  input' <- readFile "day17.txt"
  return $ map read $ words [if c == ',' then ' ' else c | c <- input']

printScaffold :: [Integer] -> IO ()
printScaffold output = forM_ output (putChar . chr. fromIntegral)

mapScaffold :: [Integer] -> Set (Int, Int)
mapScaffold opcodes = go 0 0 Set.empty $ intcode opcodes []
  where go _ _ scaffold [] = scaffold 
        go row col scaffold (x:xs) = case x of 10 -> go (row+1) 0 scaffold xs 
                                               46 -> go row (col+1) scaffold xs 
                                               _  -> go row (col+1) (Set.insert (row,col) scaffold) xs 
                                               
isIntersection :: Set (Int, Int) -> (Int, Int) -> Bool
isIntersection scaffold (row,col) = all (flip Set.member scaffold) [(row+1,col),(row-1,col),(row,col+1),(row,col-1)]

findIntersections scaffold = Set.filter (isIntersection scaffold) scaffold

calibrateCameras = Set.foldr (\(row,col) acc -> row*col + acc) 0 . findIntersections

part1 :: [Integer] -> Int 
part1 = calibrateCameras . mapScaffold

routine,a,b,c,video :: [Integer] 
routine = map (toInteger . ord) "B,C,C,A,A,B,B,C,C,A\n"
a = map (toInteger . ord) "R,12,R,4,L,6,L,8,L,8\n"
b = map (toInteger . ord) "L,12,R,4,R,4\n"
c = map (toInteger . ord) "R,12,R,4,L,12\n"
video = map (toInteger . ord) "n\n"

part2 :: [Integer] -> Integer
part2 opcodes = last $ intcode (2:tail opcodes) $ routine ++ a ++ b ++ c ++ video
