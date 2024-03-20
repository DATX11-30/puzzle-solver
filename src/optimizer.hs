module Optimizer where

import Sudoku
import Sudokus 
import SudokuLogic
import Data.List



{----------------------
Functions for prioritating which positions to check first
----------------------}

-- | Creating the final list of positions by order of most used number and most filled block
finalOrderOfPosition :: Sudoku -> [Position]
finalOrderOfPosition sud = nub (concat [getAllPositionForValue sud sudval|sudval <- (sudValOrder (findOccuranceOfNumbers sud))])

-- [(0,0),(0,1),(1,0),(1,1),(2,0),(2,1),(3,3),(3,4),(3,5),(5,3),(5,4),(5,5),(3,6),(3,7),(3,8),(5,6),(5,7),(5,8),(6,0),(6,1),(7,0),(7,1),(8,0),(8,1),(0,3),(0,4),(0,5),(1,3),(1,4),(1,5),(2,3),(2,4),(2,5),(0,6),(0,7),(0,8),(1,6),(1,7),(1,8),(2,6),(2,7),(2,8),(6,3),(6,4),(6,5),(7,3),(7,4),(7,5),(8,3),(8,4),(8,5),(6,6),(6,7),(6,8),(7,6),(7,7),(7,8),(8,6),(8,7),(8,8)]

-- | Returning all positions in each block  
blocksPos :: [[Position]]
blocksPos  = [[(r+i,c+j) |  i <- [0..2], j <- [0..2]] | r <- [0,3,6], c <- [0,3,6]]

-- | Returns all posible positions for a value to be placed  
getAllPositionForValue :: Sudoku -> SudVal -> [Position]
getAllPositionForValue sud val = concat $ sortOn (length) (getPos' blocksPos) 
    where   getPos' :: [[Position]] -> [[Position]]
            getPos' [] = []
            getPos' (s:ss) = (filter (\x -> elem val (getCandidates sud x) && (not (isFilled (valFromPos sud x)))) s) : (getPos' ss)

-- | Order all sudval after a list of ints
sudValOrder :: [Int] -> [SudVal]
sudValOrder list = [snd x |x <-(sortOn fst combine)]
    where combine = zip list [One .. Nine]
          
-- | Finds how many of each value is placed in the sudoku
findOccuranceOfNumbers :: Sudoku -> [Int]
findOccuranceOfNumbers sud = [findOccuranceOfNumber i sud | i<-r]
    where r = map Filled [One ..]

-- |Finds how many time a number is placed in a sudoku
findOccuranceOfNumber :: Value-> Sudoku -> Int
findOccuranceOfNumber val sud = length (filter (\x -> x == val) sudlist) 
    where 
        sudlist = concat sud



{------------
Functions for optimising the teqniques used when
------------}