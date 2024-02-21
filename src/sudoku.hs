module Sudoku where
import GHC.Arr (fill)
import Data.List 

-- | A representation of the values in a sudoku from 0-9 TODO: SHOULD BE RENAMED!!!!!
data SudVal = One | Two | Three | Four | Five | Six | Seven | Eight | Nine
    deriving (Eq, Enum, Bounded) 

instance Show SudVal where
    show One = "1"
    show Two = "2"
    show Three = "3"
    show Four = "4"
    show Five = "5"
    show Six = "6"
    show Seven = "7"
    show Eight = "8"
    show Nine = "9"

data Value
    = Filled SudVal 
    | Note [SudVal]
    | Empty
    deriving (Eq)

instance Show Value where
    show (Filled x) = show x
    show (Note xs) = show xs
    show Empty = " "
    
-- | A representation of a sudoku [Rows] 
type Sudoku = [[Value]]

-- | a position in a soduku (Row, Col)
type Position = (Int, Int)

-- | A secton is a collection of 9 values, either a row, column or block
type Section =  [Value]

-- | A Row is a collection of 9 Values sorted from left to right
type Row =  Section

-- | A column is a collection of 9 Values sorted from top to bottom
type Column =  Section

-- | A block is a collection of 9 Values sorted from top left to bottom right
type Block =  Section

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

-- | Fills a cell with a value
fillCell :: Sudoku -> Position -> Value -> Sudoku
fillCell sud (row, col) v = take row sud ++ [changeAtIndex (sud !! row) col v] ++ drop (row+1) sud
    where
        changeAtIndex :: [a] -> Int -> a -> [a]
        changeAtIndex (x:xs) p v | p == 0 = v:xs
                         | otherwise = x : changeAtIndex xs (p-1) v

-- | Property for fillCell, checks if cell filled with its original value is the same as the original sudoku
prop_fillCell :: Sudoku -> Position -> Bool
prop_fillCell sud (row, col) = fillCell sud (row,col) (sud !! row !! col) == sud

-- | Property for fillCell, checks if sudoku with cell filled with a value has that value at the correct position
prop_fillCell2 :: Sudoku -> Position -> Value -> Bool
prop_fillCell2 sud (row, col) v = fillCell sud (row,col) v !! row !! col == v
        

-- | Returns the value at the given position
valFromPos :: Sudoku -> (Int, Int) -> Value
valFromPos sud (row, col) = sud !! row !! col

-- | Returns all rows
rows :: Sudoku ->  [Row]
rows sud = sud

-- | Returns all colums
columns :: Sudoku ->  [Column]
columns = transpose

-- | Returns all blocks
blocks :: Sudoku ->  [Block]
blocks sud = [[valFromPos sud (r+i,c+j) |  i <- [0..2], j <- [0..2]] | r <- [0,3,6], c <- [0,3,6]] 

-- | This is for seeing whether the blocks works correctly. It is not very constructive..
prop_blocks :: Bool
prop_blocks = blocks emptySudoku == replicate 9 (replicate 9 Empty) && blocks illegalSudoku == [[Filled x | _ <- [1..9]] | x <- [One ..]]

-- | Get the row that this position is included in
rowFromPos :: Sudoku -> Position -> Row
rowFromPos sud (row, col) = sud !! row

-- | Get the column that this position is included in
colFromPos :: Sudoku -> Position -> Column
colFromPos sud (row, col) = transpose sud !! col

-- | Get the block that this position is included in
blockFromPos :: Sudoku -> Position -> Block 
blockFromPos sud (row, col) = [valFromPos sud (r+i,c+j) |  i <- [0..2], j <- [0..2]]
    where
        r = (row `div` 3) * 3
        c = (col `div` 3) * 3

-- | Gets the row, column and block associated with the position. 
sectionsFromPos :: Sudoku -> Position -> [Section]
sectionsFromPos sud pos = [rowFromPos sud pos, colFromPos sud pos, blockFromPos sud pos]

-- | Show a soduku in a more readable way. 
-- | for correct formating do not forget putStrLn before calling this function
showSudoku :: Sudoku -> String
showSudoku [] = ""
showSudoku (r1:r2:r3:rs) = showR r1 ++ "\n" ++ showR r2  ++ "\n" ++  showR r3 ++ "\n" ++ "-----------------------" ++ "\n" ++ showSudoku rs
        where   showR :: [Value] -> String
                showR [] = "||"
                showR (x1:x2:x3:xs) = "||" ++ show x1 ++ " " ++ show x2 ++ " " ++ show x3 ++ showR xs


