import Test.QuickCheck
import IO
import Logic
import Solver
import Sudoku
import SudokuLogic
import Sudokus
import Optimizer
import Data.List

-- Test function for checking that it works
prop_reverse :: [Int] -> Bool
prop_reverse xs = reverse (reverse xs) == xs

--
main :: IO ()
main = return ()