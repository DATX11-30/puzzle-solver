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
lastFreeCellInSection s pos sec = oneCellInSection s sec && not (isFilled $ valFromPos s pos) 

oneCellInSection :: Sudoku -> [Value] -> Bool
oneCellInSection s vs = length (filter isFilled vs) == 8 

isFilled :: Value -> Bool
isFilled (Filled _) = True
isFilled _ = False
