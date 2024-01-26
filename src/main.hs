import SudokuLogic 
import Test.QuickCheck

-- Test function for checking that it works
prop_reverse :: [Int] -> Bool
prop_reverse xs = reverse (reverse xs) == xs


-- 
main:: a -> a
main x = null