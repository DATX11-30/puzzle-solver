module Sudoku where
import GHC.Arr (fill)

-- | A representation of the values in a sudoku from 0-9 TODO: SHOULD BE RENAMED!!!!!
data SudVal = One | Two | Three | Four | Five | Six | Seven | Eight | Nine
    deriving (Eq, Show, Enum, Bounded) 

data Value
    = Filled SudVal 
    | Note [SudVal]
    | Empty
    deriving (Eq, Show)
    
-- | A representation of a sudoku
type Sudoku = [[Value]]

-- | a position in a soduku
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
fillCell = error "Not implemented"

-- | Returns the value at the given position
valFromPos :: Sudoku -> (Int, Int) -> Value
valFromPos = error "Not implemented"

-- | Returns all rows
rows :: Sudoku ->  [Row]
rows = error "Not implemented"

-- | Returns all colums
columns :: Sudoku ->  [Column]
columns = error "Not implemented"

-- | Returns all blocks
blocks :: Sudoku ->  [Block]
blocks = error "Not implemented"

-- | Get the row that this position is included in
rowFromPos :: Sudoku -> Position -> Row
rowFromPos = error "Not implemented"

-- | Get the column that this position is included in
colFromPos :: Sudoku -> Position -> Column
colFromPos = error "Not implemented"

-- | Get the block that this position is included in
blockFromPos :: Sudoku -> Position -> Block 
blockFromPos = error "Not implemented"

-- | Gets the row, column and block associated with the position. 
sectionsFromPos :: Sudoku -> Position -> [Section]
sectionsFromPos = error "Not implemented"
