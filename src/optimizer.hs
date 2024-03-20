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