module Optimizer where
import Sudoku
import Sudokus 
import SudokuLogic
import Data.List



{----------------------
Functions for prioritating which positions to check first
----------------------}
-- Optim    MFB     LFB     EmptyPos
-- s1       1910    1889    2089
-- s2       344     302     501
-- s3       36753   36951   47583

-- | Creating the final list of positions by order of most used number and most filled block
finalOrderOfPosition :: Sudoku -> [Position]
finalOrderOfPosition sud = reverse (nub (concat [getAllPositionForValue sud sudval|sudval <- (sudValOrder (findOccuranceOfNumbers sud))]))

-- | Returning all positions in each block  
blocksPos :: [[Position]]
blocksPos  = [[(r+i,c+j) |  i <- [0..2], j <- [0..2]] | r <- [0,3,6], c <- [0,3,6]]

-- | Returns all posible positions for a value to be placed  
getAllPositionForValue :: Sudoku -> SudVal -> [Position]
getAllPositionForValue sud val = concat $ (reverse(sortOn (length) (getPos' blocksPos))) 
    where   getPos' :: [[Position]] -> [[Position]]
            getPos' [] = []
            getPos' (s:ss) = (filter (\x -> elem val (getCandidates sud x) && (not (isFilled (valFromPos sud x)))) s) : (getPos' ss)

-- | Order all sudval after a list of ints
sudValOrder :: [Int] -> [SudVal]
sudValOrder list = [snd x |x <-(sortOn fst combine)]
    where combine = zip list [One .. Nine]
          
-- | Finds how many of each value is placed in the sudoku
findOccuranceOfNumbers :: Sudoku -> [Int]
findOccuranceOfNumbers sud = [findOccuranceOfNumber i sud | i<-r]
    where r = map Filled [One ..]

-- |Finds how many time a number is placed in a sudoku
findOccuranceOfNumber :: Value-> Sudoku -> Int
findOccuranceOfNumber val sud = length (filter (\x -> x == val) sudlist) 
    where 
        sudlist = concat sud



{------------
Functions for optimising the teqniques used when
------------}

-- |Sorts a list of tupels by the first value in the tuple
sudStepOrder :: [(Int,a)] ->[a]
sudStepOrder list = [snd x |x <-(sortOn fst list)] 


-- | Creates a list of weight to utilize when sorting the steps to test first
stepWeight :: Sudoku -> [Int]
stepWeight sud = [
    --{-LastFreeCellBlock     -} 1500 - (information !! 0) * 110,
    --{-LastFreeCellRow       -} 1500 - (information !! 1) * 110,
    --{-LastFreeCellCollumn   -} 1500 - (information !! 2) * 110,
    {-SingleCandidate       -} 1000,
    {-SingelPositionRow     -} 1500 - (information !! 3) * 120,
    {-SinglePositionColumn  -} 1500 - (information !! 4) * 120,
    {-SinglePositionBlock   -} 1500 - (information !! 5) * 120,
    {-CandidateLine         -} 16000,
    {-NakedPairRow          -} 2000000 - (information !! 6) * (information !! 4) * 5,
    {-NakedPairColumn       -} 2000000 - (information !! 6) * (information !! 5) * 5,
    {-NakedPairBlock        -} 2000000 - (information !! 6) * (information !! 3) * 5,
    {-HiddenPairBlock       -} 2000000, 
    {-HiddenPairRow         -} 2000001,
    {-HiddenPairColumn      -} 2000002,
    {-NOAVAILABLESTEPS      -} 3000000000
    ]
    where
        information = informationForWeight sud

{-
                CandidateLine Position          |
                NakedPairRow Position           |
                NakedPairColumn Position        |
                NakedPairBlock Position         |
                NOAVAILABLESTEPS
    deriving (Eq,Show) -- | Then add more step types
-}

-- | Function that creates a list of values from a sudoku to be utilised in determening the weights
informationForWeight :: Sudoku -> [Int]
informationForWeight sud = [maxBlocks,maxRows,maxCols,maxRowsWNotes,maxColsWNotes,maxBlocksWNotes,maxFilledNumber]
    where
        maxRows = mostFilledSection sud
        maxCols = mostFilledSection $ columns sud
        maxBlocks = mostFilledSection $ blocks sud
        maxFilledNumber = maximum $ (filter (\x-> x /= 9) ((findOccuranceOfNumbers sud) ++ [0] ) )
        maxRowsWNotes = mostFilledSectionWNotes sud
        maxColsWNotes = mostFilledSectionWNotes $ columns sud
        maxBlocksWNotes = mostFilledSectionWNotes $ blocks sud


-- | Returns how filled the most non full section is 
mostFilledSection :: [Section] -> Int
mostFilledSection sections = maximum $ filter (\x-> x /= 9) (filledInSections sections)

-- | Takes a list of section and return how filled in each section not counting notes
filledInSections :: [Section] -> [Int]
filledInSections sections = (map filledSection sections)

-- | Returns how filled a section is not counting notes as filled
filledSection :: Section -> Int
filledSection [] = 0
filledSection ((Filled _):sp) = 1 + filledSection sp
filledSection (_:sp) = 0 + filledSection sp

-- | Returns how filled the most non full section is
mostFilledSectionWNotes :: [Section] -> Int
mostFilledSectionWNotes sections = maximum $ filter (\x-> x /= 9) (filledInSectionsWNotes sections ++ [0])

-- | Takes a list of section and return how filled in each section is counting notes
filledInSectionsWNotes :: [Section] -> [Int]
filledInSectionsWNotes sections = (map filledSectionWNotes sections) ++ [0]

-- | Returns how filled a section is counting notes as filled
filledSectionWNotes :: Section -> Int
filledSectionWNotes [] = 0
filledSectionWNotes ((Empty):sp) = 0 + filledSection sp
filledSectionWNotes (_:sp) = 1 + filledSection sp