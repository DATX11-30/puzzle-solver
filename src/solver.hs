module Solver where

import Sudoku
import SudokuLogic
import Data.Monoid (Last(Last))


data Step = LastFreeCell Position | LastRemainingCell Position | BlockedByRow Position |  NOAVALIBLESTEPS | TwoRemainingCells Position
    deriving (Eq,Show) -- | Then add more step types

type Soulution = [Step]

steps :: Position -> [Step]
steps p = [LastFreeCell p, LastRemainingCell p, BlockedByRow p]

-- | A solver for sudoku

solve :: Sudoku -> Soulution -> Maybe Sudoku
solve s = error "Not implemented"

-- | Generates a soulution
generateSoulution :: Sudoku -> Soulution
generateSoulution = error "Not implemented"

-- | next step in solving the sudoku
nextStep :: Sudoku -> Step
nextStep = error "Not implemented"

testSteps :: Sudoku -> [Step] -> Step
testSteps _ [] = NOAVALIBLESTEPS
testSteps sud ((LastFreeCell p):xs) | lemmaLastCellInBlock sud p = LastFreeCell p
                                    | otherwise = testSteps sud xs
                                    
