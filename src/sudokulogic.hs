module SudokuLogic where

import Sudoku
{-
data Either a b = Left a | Right b

-}

lemmaLastInRow :: Sudoku -> Position -> Bool
lemmaLastInRow s pos = lastFreeCellInSection s pos (rowFromPos s pos)

lemmaLastInCol :: Sudoku -> Position -> Bool
lemmaLastInCol s pos = lastFreeCellInSection s pos (colFromPos s pos)

lemmaLastInBlock :: Sudoku -> Position -> Bool
lemmaLastInBlock s pos = lastFreeCellInSection s pos (blockFromPos s pos)

-- | if a section only has one empty cell, then that cell must be filled with the only possible value
lastFreeCellInSection :: Sudoku -> Position -> Section -> Bool
lastFreeCellInSection s pos sec = oneCellInSectionV s sec && not (isFilled $ valFromPos s pos) 


missingValues :: [Value] -> [Value]
missingValues vs = [Filled x | x <- [One .. Nine], Filled x `notElem` vs]

oneCellInSection :: Sudoku -> [(Int, Int)] -> Bool
oneCellInSection s vs = length (filter (isFilled.valFromPos s) vs) == 8 


oneCellInSectionV :: Sudoku -> [Value] -> Bool
oneCellInSectionV s vs = length (filter isFilled vs) == 8 

emptyPositions :: Sudoku -> [(Int, Int)]
emptyPositions s = filter (not . isFilled . valFromPos s) positions
    where
    positions :: [(Int, Int)]
    positions = [(x,y) | x <- [1..9], y <- [1..9]] 



isFilled :: Value -> Bool
isFilled (Filled _) = True
isFilled _ = False
