module Sudoku where

-- | A representation of the values in a sudoku from 0-9 TODO: SHOULD BE RENAMED!!!!!
data SudVal = One | Two -- ... 

data Value
    = Filled SudVal 
    | Note [SudVal]
    | Empty
    
-- | A representation of a sudoku
type Sudoku = [[Value]]



-- | a position in a soduku
type Position = (Int, Int)

type Row = [Value]

type Column = [Value]

type Block = [Value]

-- | Returns all rows
rows :: Sudoku ->  [Row]
rows = undefined

-- | Returns all colums
columns :: Sudoku ->  [Column]
columns = undefined

-- | Returns all blocks
blocks :: Sudoku ->  [Block]
blocks = undefined

-- | Get the row that this position is included in
rowFromPos :: Sudoku -> Position -> Row
rowFromPos = undefined

-- | Get the column that this position is included in
colFromPos :: Sudoku -> Position -> Column
colFromPos = undefined

-- | Get the block that this position is included in
blockFromPos :: Sudoku -> Position -> Block 
blockFromPos = undefined


