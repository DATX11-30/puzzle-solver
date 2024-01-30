module SudokuLogic where

import Test.QuickCheck

prop_reverse2 :: [Int] -> Bool
prop_reverse2 xs = reverse (reverse xs) == xs