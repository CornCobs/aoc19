module Day19 where

import ST.IntCodeST
import Control.Monad

scanArea :: [Integer] -> [(Integer, Integer, Integer)]
scanArea opcodes = do 
  x <- [0..49]
  y <- [0..49]
  return (x,y, head $ intcode opcodes [x,y])
  
printArea :: [Integer] -> IO ()
printArea opcodes =
  forM_ [0..49] $ \y -> do 
    forM_ [0..49] $ \x -> 
      if intcode opcodes [x,y] == [0]
      then putStr "."
      else putStr "#"
    putStrLn ""
  
day19 :: IO [(Integer, Integer, Integer)]
day19 = do 
  input <- readFile "day19.txt"
  let opcodes = map read $ words [if c == ',' then ' ' else c | c <- input]
  return $ scanArea opcodes 
  
test :: [Integer] -> Integer -> Integer -> Bool
test opcodes x y = intcode opcodes [x,y] == [1]
  
fullSquareFits :: [Integer] -> Integer -> Integer -> Bool 
fullSquareFits opcodes x y = test opcodes x y && test opcodes (x-99) (y+99)

squareSearch :: [Integer] -> Integer
squareSearch opcodes = search (446, 500)
  where search (prevX, prevY) = 
          let y = prevY + 1
              x = last $ takeWhile (\x' -> test opcodes x' y) [prevX..]
          in  if fullSquareFits opcodes x y
              then (x-99) * 10000 + y
              else search (x, y)