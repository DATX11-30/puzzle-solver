module Optimizer where

import Sudoku
import Sudokus 
--[avRow, avCol, avBlock, maxRow, maxCol, maxBlock, ]
-- Vilka siffror är flest utplacerade
-- Titta på sectioner summer hur många platser siffran kan vara på

exSudoku4 :: Sudoku
exSudoku4 = exSudoku3

findOccuranceOfNumber :: Value-> Sudoku -> Int
findOccuranceOfNumber val sud = length (filter (\x -> x == val) sudlist) 
    where 
        sudlist = concat sud

test10 :: Int
test10 = findOccuranceOfNumber (Filled One) exSudoku1