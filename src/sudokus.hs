module Sudokus where
import Sudoku
import Data.Type.Coercion (trans)

-- | This file contains some sudokus for testing purposes


-- | An empty soduku
emptySudoku :: Sudoku
emptySudoku = replicate 9 (replicate 9 Empty)

-- | An illegal sudoku for testing purposes
illegalSudoku :: Sudoku
illegalSudoku = [[Filled One, Filled One, Filled One, Filled Two, Filled Two, Filled Two, Filled Three, Filled Three, Filled Three],
                 [Filled One, Filled One, Filled One, Filled Two, Filled Two, Filled Two, Filled Three, Filled Three, Filled Three],
                 [Filled One, Filled One, Filled One, Filled Two, Filled Two, Filled Two, Filled Three, Filled Three, Filled Three],
                 [Filled Four, Filled Four, Filled Four, Filled Five, Filled Five, Filled Five, Filled Six, Filled Six, Filled Six],
                 [Filled Four, Filled Four, Filled Four, Filled Five, Filled Five, Filled Five, Filled Six, Filled Six, Filled Six],
                 [Filled Four, Filled Four, Filled Four, Filled Five, Filled Five, Filled Five, Filled Six, Filled Six, Filled Six],
                 [Filled Seven, Filled Seven, Filled Seven, Filled Eight, Filled Eight, Filled Eight, Filled Nine, Filled Nine, Filled Nine],
                 [Filled Seven, Filled Seven, Filled Seven, Filled Eight, Filled Eight, Filled Eight, Filled Nine, Filled Nine, Filled Nine],
                 [Filled Seven, Filled Seven, Filled Seven, Filled Eight, Filled Eight, Filled Eight, Filled Nine, Filled Nine, Filled Nine]
                ]


-- | Sudoku missing the to row 
legalSudoku :: Sudoku
legalSudoku = [[Filled Two, Filled Five, Filled Six, Filled Eight, Filled Three, Filled Seven, Filled One, Filled Four, Filled Nine],
                 [Filled Seven, Filled One, Filled Nine, Filled Four, Filled Two, Filled Five, Filled Eight, Filled Three, Filled Six],
                 [Filled Eight, Filled Four, Filled Three, Filled Six, Filled One, Filled Nine, Filled Two, Filled Five, Filled Seven],
                 [Filled Four, Filled Six, Filled Seven, Filled One, Filled Five, Filled Eight, Filled Nine, Filled Two, Filled Three],
                 [Filled Three, Filled Nine, Filled Two, Filled Seven, Filled Six, Filled Four, Filled Five, Filled One, Filled Eight],
                 [Filled Five, Filled Eight, Filled One, Filled Three, Filled Nine, Filled Two, Filled Six, Filled Seven, Filled Four],
                 [Filled One, Filled Seven, Filled Eight, Filled Two, Filled Four, Filled Six, Filled Three, Filled Nine, Filled Five],
                 [Filled Six, Filled Three, Filled Five, Filled Nine, Filled Seven, Filled One, Filled Four, Filled Eight, Filled Two],
                 [Filled Nine, Filled Two, Filled Four, Filled Five, Filled Eight, Filled Three, Filled Seven, Filled Six, Filled One]
                ]

exSudoku1 :: Sudoku
exSudoku1 = [[Empty, Empty, Empty, Filled Eight, Empty, Empty, Empty, Empty, Filled Nine],
             [Empty, Filled One, Filled Nine, Empty, Empty, Filled Five, Filled Eight, Filled Three, Empty],
             [Empty, Filled Four, Filled Three, Empty, Filled One, Empty, Empty, Empty, Filled Seven],
             [Filled Four, Empty, Empty, Filled One, Filled Five, Empty, Empty, Empty, Filled Three],
             [Empty, Empty, Filled Two, Filled Seven, Empty, Filled Four, Empty, Filled One, Empty],
             [Empty, Filled Eight, Empty, Empty, Filled Nine, Empty, Filled Six, Empty, Empty],
             [Empty, Filled Seven, Empty, Empty, Empty, Filled Six, Filled Three, Empty, Empty],
             [Empty, Filled Three, Empty, Empty, Filled Seven, Empty, Empty, Filled Eight, Empty],
             [Filled Nine, Empty, Filled Four, Filled Five, Empty, Empty, Empty, Empty, Filled One]]

exSudoku2 :: Sudoku
exSudoku2 = [[Empty, Empty, Filled Four, Empty, Empty, Filled Six, Empty, Filled Two, Empty],
             [Empty, Empty, Filled Seven, Filled Eight, Empty, Empty, Filled Nine, Filled One, Empty],
             [Empty, Empty, Empty, Empty, Empty, Empty, Filled Three, Empty, Filled Eight],
             [Empty, Filled One, Filled Eight, Filled Three, Empty, Empty, Filled Two, Empty, Empty],
             [Filled Three, Empty, Empty, Filled Seven, Filled Eight, Filled Nine, Empty, Empty, Filled One],
             [Empty, Empty, Filled Nine, Empty, Empty, Filled One, Empty, Filled Six, Empty],
             [Filled Eight, Empty, Filled Three, Empty, Empty, Empty, Filled Five, Empty, Empty],
             [Empty, Filled Four, Filled Five, Empty, Empty, Filled Three, Filled Six, Empty, Empty],
             [Empty, Filled Two, Filled Six, Filled Five, Empty, Empty, Filled One, Empty, Empty]]

exSudoku3 :: Sudoku
exSudoku3 = [[Empty, Filled Four, Filled Seven, Empty, Empty, Filled Six, Empty, Empty, Filled Three],
             [Empty, Empty, Empty, Empty, Empty, Filled Four, Empty, Filled Eight, Empty],
             [Empty, Empty, Filled Eight, Empty, Filled Seven, Filled One, Empty, Empty, Empty],
             [Empty, Empty, Filled Three, Empty, Empty, Empty, Filled Five, Filled Six, Empty],
             [Empty, Empty, Empty, Empty, Filled Nine, Empty, Empty, Empty, Empty],
             [Empty, Filled Five, Filled Nine, Empty, Empty, Empty, Filled Eight, Empty, Empty],
             [Empty, Empty, Empty, Filled One, Filled Six, Empty, Filled Seven, Empty, Empty],
             [Empty, Filled Eight, Empty, Filled Three, Empty, Empty, Empty, Empty, Empty],
             [Filled Nine, Empty, Empty, Filled Four, Empty, Empty, Filled One, Filled Five, Empty]]

missingTopRowSud :: Sudoku
missingTopRowSud = [[Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty],
                 [Filled Seven, Filled One, Filled Nine, Filled Four, Filled Two, Filled Five, Filled Eight, Filled Three, Filled Six],
                 [Filled Eight, Filled Four, Filled Three, Filled Six, Filled One, Filled Nine, Filled Two, Filled Five, Filled Seven],
                 [Filled Four, Filled Six, Filled Seven, Filled One, Filled Five, Filled Eight, Filled Nine, Filled Two, Filled Three],
                 [Filled Three, Filled Nine, Filled Two, Filled Seven, Filled Six, Filled Four, Filled Five, Filled One, Filled Eight],
                 [Filled Five, Filled Eight, Filled One, Filled Three, Filled Nine, Filled Two, Filled Six, Filled Seven, Filled Four],
                 [Filled One, Filled Seven, Filled Eight, Filled Two, Filled Four, Filled Six, Filled Three, Filled Nine, Filled Five],
                 [Filled Six, Filled Three, Filled Five, Filled Nine, Filled Seven, Filled One, Filled Four, Filled Eight, Filled Two],
                 [Filled Nine, Filled Two, Filled Four, Filled Five, Filled Eight, Filled Three, Filled Seven, Filled Six, Filled One]
                ]

missingLeftColSud :: Sudoku
missingLeftColSud = columns missingTopRowSud