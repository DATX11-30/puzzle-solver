module SudokuLogic where

import Sudoku
import GHC.Arr (fill)

type Lemma = Sudoku -> Position -> Bool

-- | A sorted list of all the lemmas avalible sorted in a ~smart~ order	 
lemmas :: [Lemma]
lemmas = [lemmaLastCellInBlock,
          lemmaLastCellInColumn,
          lemmaLastCellInBlock]


-- | A lemma that checks if a value is the last in a block
lemmaLastCellInBlock :: Sudoku -> Position -> Bool
lemmaLastCellInBlock s pos = lastFreeCellInSection s pos (blockFromPos s pos)

-- | A lemma that checks if a value is the last in a row
lemmaLastCellInRow :: Sudoku -> Position -> Bool
lemmaLastCellInRow s pos = lastFreeCellInSection s pos (rowFromPos s pos)

-- | A lemma that checks if a value is the last in a column
lemmaLastCellInColumn :: Sudoku -> Position -> Bool
lemmaLastCellInColumn s pos = lastFreeCellInSection s pos (colFromPos s pos)

-- | if a section only has one empty cell, then that cell must be filled with the only possible value
lastFreeCellInSection :: Sudoku -> Position -> Section -> Bool
lastFreeCellInSection s pos sec = oneCellInSection s sec && not (isFilled $ valFromPos s pos) 

prop_lastCell :: Position -> Bool
prop_lastCell (row, col) = lemmaLastCellInRow (fillCell illegalSudoku (row, col) Empty) (row, col) && 
                           lemmaLastCellInColumn (fillCell illegalSudoku (row, col) Empty) (row, col) && 
                           lemmaLastCellInBlock (fillCell illegalSudoku (row, col) Empty) (row, col)

testSud :: Sudoku
testSud = illegalSudoku 


oneCellInSection :: Sudoku -> [Value] -> Bool
oneCellInSection s vs = length (filter isFilled vs) == 8 

isFilled :: Value -> Bool
isFilled (Filled _) = True
isFilled _ = False












