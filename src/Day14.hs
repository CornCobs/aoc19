{-# LANGUAGE TypeApplications, BangPatterns #-}

module Day14 where

import Data.List
import Data.Maybe
import qualified Data.Map.Strict as Map 
import Data.Map.Strict (Map, (!))
import Data.Graph 

import ST.STPrograms (chunksOf)

data Equation = Equation { inputs :: Map String Integer, quantity :: Integer }
type Output = String 

parseEquation :: String -> (Equation, Output)
parseEquation eqn = 
  let removeCommas = filter (/= ',')
      tokens = map removeCommas $ words eqn 
      (inps, [_,outQ',out]) = splitAt (length tokens - 3) tokens 
      outQ = read @Integer outQ'
      input = foldr (\[inpQ,inp] acc -> Map.insert inp (read @Integer inpQ) acc) Map.empty $ chunksOf 2 inps 
  in  (Equation input outQ, out)
  
parseEquations :: [String] -> Map Output Equation
parseEquations eqns = foldr (\eqn acc -> let (inp, out) = parseEquation eqn in Map.insert out inp acc) Map.empty eqns 
  
-- builds a graph from outputs to inputs, i.e. if A is reachable from B (i.e. path graph B A) then A is made from B
buildGraph :: Map Output Equation -> (Graph, Vertex -> (String, String, [String]), String -> Maybe Vertex)
buildGraph eqns = graphFromEdges $ Map.foldrWithKey (\out (Equation inp _) acc -> (out, out, Map.keys inp):acc) [("ORE","ORE",[])] eqns

calculateOREwithExcess :: Integer -> [String] -> Map Output Equation -> Integer
calculateOREwithExcess needed topoOrder eqns = 
  let produce :: Integer -> String -> Map String Integer -> (Integer, Map String Integer) 
      produce n "ORE" currLeftovers = (n, currLeftovers)
      produce n chemical currLeftovers = 
        let Equation inp q = eqns ! chemical 
            (sets, leftover) = minSetsToProduceAndRem n q 
            inputlist = filter (flip Map.member inp) topoOrder 
        in  if Map.member chemical currLeftovers 
            then if n <= currLeftovers ! chemical 
                 then (0, Map.adjust (+ (-n)) chemical currLeftovers)
                 else produce (n - currLeftovers ! chemical) chemical $ Map.delete chemical currLeftovers
            else foldr (\chem (ore, leftovers) -> let (ore', leftovers') = produce (sets * inp ! chem) chem leftovers 
                                                  in (ore + ore', leftovers'))
                 (0, Map.insert chemical leftover currLeftovers)
                 inputlist
                 
  in fst $ produce needed "FUEL" Map.empty
  
underProduced :: Integer -> [String] -> Map Output Equation -> Bool
underProduced n topoOrder eqns = calculateOREwithExcess (n + 1) topoOrder eqns < 10^12
  
maxFUEL :: Map Output Equation -> Integer 
maxFUEL eqns = binSearch 6000000 7000000
  where (graph, verttoNode, nodetoVert) = buildGraph eqns 
        topoOrder = map (\v -> let (x,_,_) = verttoNode v in x) $ topSort graph
        binSearch low high 
          | midOREreq > 10^12 = binSearch low (mid-1) 
          | underProduced mid topoOrder eqns = binSearch (mid + 1) high 
          | otherwise = mid 
          where mid = (low + high) `div` 2
                midOREreq = calculateOREwithExcess mid topoOrder eqns 

minSetsToProduceAndRem :: Integer -> Integer -> (Integer, Integer)
minSetsToProduceAndRem needed batch = if needed `mod` batch == 0 then (sets, 0) else (sets + 1, (sets + 1) * batch - needed)
  where sets = needed `div` batch

day14 :: IO ()
day14 = do 
  input <- readFile "day14.txt"
  print $ maxFUEL $ parseEquations $ lines input
  