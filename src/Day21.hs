module Day21 where 

import ST.IntCodeST
import Data.Char

getOpcodes :: IO [Integer]
getOpcodes = do 
  input <- readFile "day21.txt"
  return $ map read $ words [if c == ',' then ' ' else c | c <- input]
  
springdroid :: [Integer] -> IO ()
springdroid opcodes = do 
  let output = intcode opcodes $ map (toInteger.ord) runningScript
      safeChr i | fromIntegral i > ord maxBound = show i 
                | otherwise = [chr . fromIntegral $ i]
  putStrLn (concat $ map safeChr output)
                         
                        
      
springscript :: String 
springscript = unlines 
               [ "NOT T T" -- set T to True (identity under conjunction)
               , "AND A T"
               , "AND B T"
               , "AND C T" -- T will be false if hole in A B C
               , "NOT T J"
               , "AND D J"                       
               , "WALK"
               ]
                       
-- Idea: if 4th is ground, but 5th and 8th hole - dont jump. else jump 
runningScript :: String 
runningScript = unlines 
                [ "NOT J J"
                , "AND A J"
                , "AND B J"
                , "AND C J" -- J false if there is a hole in A B C
                , "NOT J J"
                , "AND D J" -- J is now true if there is a hole in A B C and D is ground
                , "OR E T"
                , "OR H T" -- T false if both holes 
                , "AND T J" -- dont jump if both 5th and 8th holes, though 4th is ground                 
                , "RUN"
                ]