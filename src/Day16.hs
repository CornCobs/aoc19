{-# LANGUAGE BangPatterns #-}

module Day16 where 

import Debug.Trace

genPattern :: Int -> [Int]
genPattern n = tail . cycle $ [0,1,0,-1] >>= replicate n

phase :: [Int] -> [Int]
phase input = take (length input) $ map (\i -> 
  (`mod` 10) . abs . sum $ zipWith (*) (genPattern i) input) [1..]
  
stepN :: ([Int] -> [Int]) -> Int -> [Int] -> [Int]
stepN f 0 input = input 
stepN f n input = stepN f (n-1) (f input)

day16 :: IO ()
day16 = do 
  input' <- readFile "day16.txt"
  let input = map (read . return) input'
  print $ stepN phase 100 input
  
  
realSignal :: [Int] -> [Int]
realSignal input = 
  let msgOffset = foldl1 (\acc n -> acc * 10 + n) (take 7 input)
      signalReversed = reverse $ drop msgOffset $ take (length input * 10000) $ cycle input
      stepReversed acc [] = trace "done" [] 
      stepReversed !acc (x:xs) = (x + acc) `mod` 10 : stepReversed (x + acc) xs
      finalOutput = take 8 $ reverse $ stepN (stepReversed 0) 100 signalReversed 
  in finalOutput