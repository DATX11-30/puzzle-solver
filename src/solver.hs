module Solver where

import Sudoku

data Step = LastFreeCell Position | LastRemainingCell Position
    deriving (Eq,Show) -- | Then add more step types

type Soulution = [Step]

-- | A solver for sudoku

solve :: Sudoku -> Soulution
solve s = error "Not implemented"

-- | next step in solving the sudoku
nextStep :: Sudoku -> Step
nextStep s = error "Not implemented"

