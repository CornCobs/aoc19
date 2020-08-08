module Day25 where 

import Day9 hiding (inputOp, outputOp)
import qualified Data.Vector.Mutable as MV 
import Control.Monad
import Data.Char

getOpcodes :: IO [Integer]
getOpcodes = do 
  input <- readFile "day25.txt"
  return $ map read $ words [if c == ',' then ' ' else c | c <- input]

intcode :: [Integer] -> IO ()
intcode opcodes = do
  program <- MV.replicate 10000 0
  forM_ (zip [0..] opcodes) $ \(ix, opcode) -> MV.write program ix opcode
  let process relbase pos = do 
        code <- MV.read program pos 
        case code `mod` 100 of 
          1 -> mathOp (+) relbase code pos program >> process relbase (pos + 4)
          2 -> mathOp (*) relbase code pos program >> process relbase (pos + 4)
          3 -> inputOp    relbase code pos program >> process relbase (pos + 2)
          4 -> outputOp   relbase code pos program >> process relbase (pos + 2)
          5 -> jumpOp (/= 0) relbase code pos program >>= process relbase 
          6 -> jumpOp (== 0) relbase code pos program >>= process relbase
          7 -> compOp (<)  relbase code pos program >> process relbase (pos + 4)
          8 -> compOp (==) relbase code pos program >> process relbase (pos + 4)
          9 -> relBaseOffsetOp relbase code pos program >>= (\newBase -> process newBase (pos + 2))
          99 -> return ()
  process 0 0
  
inputOp :: Integer -> Integer -> Int -> MV.IOVector Integer -> IO ()
inputOp relbase code pos program = do 
  input <- toInteger . ord <$> getChar
  targetPos <- if (code `mod` 1000) `div` 100 == 0 
               then fromIntegral <$> MV.read program (pos + 1)
               else (fromIntegral . (+relbase)) <$> MV.read program (pos + 1)
  MV.write program targetPos input
  
outputOp :: Integer -> Integer -> Int -> MV.IOVector Integer -> IO ()
outputOp relbase code pos program = 
  getVal 1 relbase code pos program >>= (putChar . chr . fromIntegral)