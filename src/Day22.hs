{-# LANGUAGE ViewPatterns #-}

module Day22 where

import Control.Monad.ST
import Control.Monad
import qualified Data.Vector.Mutable as MV 
import Data.Vector (toList, freeze)
import Data.List

type Deck = [Int]

initialDeck = [0..10006]
deckLength = 10007

dealNewStack :: Deck -> Deck 
dealNewStack = reverse 

cut :: Int -> Deck -> Deck 
cut n deck = back ++ front
  where n' = if n > 0 then n else n + deckLength
        (front,back) = splitAt n' deck
        
dealIncrN :: Int -> Deck -> Deck
dealIncrN n deck = runST $ do 
  table <- MV.new deckLength
  foldM_ (\pos val -> do
    MV.write table pos val
    return $ (pos + n) `mod` deckLength) 
    0
    deck
  toList <$> freeze table
  
parseInstruction :: String -> (Deck -> Deck)
parseInstruction "deal into new stack" = dealNewStack
parseInstruction (stripPrefix "cut " -> Just val) = cut (read val)
parseInstruction (stripPrefix "deal with increment " -> Just val) = dealIncrN (read val)

day22 :: IO ()
day22 = do 
  instr <- readFile "day22.txt"
  let instructions = map parseInstruction $ lines instr 
  print $ elemIndex 2019 $ foldl (\deck shuffle -> shuffle deck) initialDeck instructions