module Solver where

import Sudoku
import SudokuLogic
import Sudokus
import Data.List

data Step = LastFreeCellBlock Position |
            LastFreeCellRow Position |
            LastFreeCellCollumn Position |
            LastRemainingCell Position |
            BlockedByRow Position      |
            NOAVAILABLESTEPS
    deriving (Eq,Show) -- | Then add more step types

type Solution = [Step]

steps :: [Position -> Step]
steps = [LastFreeCellBlock, LastFreeCellRow, LastFreeCellCollumn, LastRemainingCell, BlockedByRow]

solve :: Sudoku -> Sudoku
solve sud = applySolution sud (generateSolution sud)

-- | A solver for sudoku
applySolution :: Sudoku -> Solution -> Sudoku
applySolution = foldl placeValueFromStep

-- | Generates a Solution
generateSolution :: Sudoku -> Solution
generateSolution sud = case next of
                            NOAVAILABLESTEPS -> []
                            _ -> next : generateSolution (placeValueFromStep sud next)
    where
        next = nextStep sud steps

placeValueFromStep :: Sudoku -> Step -> Sudoku
placeValueFromStep sud (LastFreeCellBlock p) = fillCell sud p (valueFromLFCB sud p)
placeValueFromStep sud (LastFreeCellRow p) = fillCell sud p (valueFromLFCR sud p)
placeValueFromStep sud (LastFreeCellCollumn p) = fillCell sud p (valueFromLFCC sud p)
placeValueFromStep sud _ = error "Not implemeted this lemma yet :("


valueFromLFCR :: Sudoku -> Position -> Value
valueFromLFCR sud pos = valueFromLFCSection (rowFromPos sud pos)

valueFromLFCC :: Sudoku -> Position -> Value
valueFromLFCC sud pos = valueFromLFCSection (colFromPos sud pos)

valueFromLFCB :: Sudoku -> Position -> Value
valueFromLFCB sud pos = valueFromLFCSection (blockFromPos sud pos)

valueFromLFCSection :: Section -> Value
valueFromLFCSection sec = case list' of
                            [x] -> x
                            _ -> error "Not a Section does not work with lemma LFCS"
    where
        list' = [Filled x | x <- [One ..]] \\ sec

-- | next step in solving the sudoku
nextStep :: Sudoku -> [Position -> Step] -> Step
nextStep sud [] = NOAVAILABLESTEPS
nextStep sud (sf:sfs) = case tryStepsOnEmpty sud sf of
                            NOAVAILABLESTEPS -> nextStep sud sfs
                            x -> x

-- | Applies tryStepOnPositions to all empty positions in a given sudoku
tryStepsOnEmpty :: Sudoku -> (Position -> Step) -> Step
tryStepsOnEmpty sud sf = tryStepOnPositions sud sf (emptyPositions sud)

-- | Returns all empty positions in a given sudoku
emptyPositions :: Sudoku -> [Position]
emptyPositions sud = [(r,c) | r <- [0..8], c <- [0..8], not (isFilled (valFromPos sud (r,c)))]

-- | Tries to apply a lemma/step to all position, 
tryStepOnPositions :: Sudoku -> (Position -> Step) -> [Position] -> Step
tryStepOnPositions sud _ [] = NOAVAILABLESTEPS
tryStepOnPositions sud sf (p:ps) | testStep sud (sf p) = sf p
                                 | otherwise = tryStepOnPositions sud sf ps

-- | Tests if a step is valid for the given sudoku
testStep :: Sudoku -> Step -> Bool
testStep sud (LastFreeCellBlock p) = lemmaLastCellInBlock sud p
testStep sud (LastFreeCellRow p) = lemmaLastCellInRow sud p
testStep sud (LastFreeCellCollumn p) = lemmaLastCellInColumn sud p
testStep sud _ = error "lemma not implemented"



