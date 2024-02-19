module SudokuLogic where

import Sudoku

type Lemma = Sudoku -> Position -> Bool

-- | A sorted list of all the lemmas avalible sorted in a ~smart~ order	 
lemmas :: [Lemma]
lemmas = [lemmaLastCellInBlock,
          lemmaLastCellInColumn,
          lemmaLastCellInBlock]


-- | A lemma that checks if a value is the last in a block
lemmaLastCellInBlock :: Sudoku -> Position -> Bool
lemmaLastCellInBlock = error "Not implemented"

-- | A lemma that checks if a value is the last in a row
lemmaLastCellInRow :: Sudoku -> Position -> Bool
lemmaLastCellInRow = error "Not implemented"

-- | A lemma that checks if a value is the last in a column
lemmaLastCellInColumn :: Sudoku -> Position -> Bool
lemmaLastCellInColumn = error "Not implemented"













