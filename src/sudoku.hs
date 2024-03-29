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

data Line = Horizontal | Vertical
    deriving (Eq)

instance Show Line where
    show Horizontal = "-"
    show Vertical = "|"

data Note = Candidate SudVal | Line Line SudVal
    deriving (Eq)

instance Show Note where
    show (Candidate x) = show x
    show (Line l x) = show l ++ show x

data Value
    = Filled SudVal
    | Note [Note]
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



-- | Fills a cell with a value
fillCell :: Sudoku -> Position -> Value -> Sudoku
fillCell sud (row, col) v = take row sud ++ [changeAtIndex (sud !! row) col v] ++ drop (row+1) sud
    where
        changeAtIndex :: [Value] -> Int -> Value -> [Value]
        changeAtIndex (x:xs) p val | p == 0 = case (val, x) of
                                            (Note a, Note b) -> if null (a \\ b) then val:xs else Note (a++b):xs
                                            _ -> val:xs
                         | otherwise = x : changeAtIndex xs (p-1) val


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

rowPositions :: Position -> [Position]
rowPositions (row, col) = [(row, c) | c <- [0..8]]

colPositions :: Position -> [Position]
colPositions (row, col) = [(r, col) | r <- [0..8]]

blockPositions :: Position -> [Position]
blockPositions (row, col) = [(r+i,c+j) |  i <- [0..2], j <- [0..2]]
    where
        r = (row `div` 3) * 3
        c = (col `div` 3) * 3


-- | Gets the row, column and block associated with the position. 
sectionsFromPos :: Sudoku -> Position -> [Section]
sectionsFromPos sud pos = [rowFromPos sud pos, colFromPos sud pos, blockFromPos sud pos]

isFilled :: Value -> Bool
isFilled (Filled _) = True
isFilled _ = False