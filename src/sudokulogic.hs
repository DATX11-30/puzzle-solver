module SudokuLogic where

import Sudoku
import Sudokus
import Data.List

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

-- | A lemma that checks if a value is the only possible value for a cell
lemmaSingleCandidate :: Sudoku -> Position -> Bool
lemmaSingleCandidate sud pos = case candidates of
                                [x] -> True
                                _ -> False
    where 
        candidates = getCandidates sud pos

prop_lastCell :: Position -> Bool
prop_lastCell (row, col) = lemmaLastCellInRow (fillCell illegalSudoku (row, col) Empty) (row, col) && 
                           lemmaLastCellInColumn (fillCell illegalSudoku (row, col) Empty) (row, col) && 
                           lemmaLastCellInBlock (fillCell illegalSudoku (row, col) Empty) (row, col)

-- | Returns all possible candidates 
getCandidates :: Sudoku -> Position -> [SudVal]
getCandidates s pos = candidates
    where 
        candidates = [x | x <- [One ..], not (elem (Filled x) occupiedVals)]
        sections = sectionsFromPos s pos
        occupiedVals = values ++ map Filled pairs
        pairs = nub $ concat $ map notePairs sections 
        values = nub $ concat sections 

-- | Testes whether lemma singele candidate is valid
prop_singleCandidate :: Position -> Bool
prop_singleCandidate (row, col) = lemmaSingleCandidate (fillCell legalSudoku (row, col) Empty) (row, col) && 
                                 not (lemmaSingleCandidate legalSudoku (row, col))


--------------------------------------------------------------------------------------------------------
-- Helper functions

-- | If the section contains two cells witch include a note with the same valus, then return those values
notePairs :: Section -> [SudVal]
notePairs sec = nub $ concat pairs
    where
        notes = filter isNote sec
        pairs = [x | (Note x) <- notes, (Note y) <- (delete (Note x) notes), x === y]

-- | Returns true if two lists are set-equal
(===) :: Eq a => [a] -> [a] -> Bool
(===) xs ys = xs \\ ys == []

isNote :: Value -> Bool
isNote (Note _) = True
isNote _ = False


testSud :: Sudoku
testSud = illegalSudoku 


oneCellInSection :: Sudoku -> [Value] -> Bool
oneCellInSection s vs = length (filter isFilled vs) == 8 














