module Optimizer where
import Sudoku
import Sudokus 
import SudokuLogic
import Data.List



{----------------------
Functions for prioritating which positions to check first
----------------------}
-- Optim    MFB     LFB     EmptyPos    MFB + Weight v.1 
-- s1       1910    1889    2089            error
-- s2       344     302     501             263 
-- s3       36753   36951   47583           252149

-- | Creating the final list of positions by order of most used number and most filled block
finalOrderOfPosition :: Sudoku -> [Position]
finalOrderOfPosition sud = nub (concat [getAllPositionForValue sud sudval|sudval <- (sudValOrder (findOccuranceOfNumbers sud))])
-- reverse $ nub (concat [getAllPositionForValue sud sudval|sudval <- (sudValOrder (findOccuranceOfNumbers sud))]) hyper optimisation
-- nub (concat [getAllPositionForValue sud sudval|sudval <- (sudValOrder (findOccuranceOfNumbers sud))]) normal 

-- | Returning all positions in each block  
blocksPos :: [[Position]]
blocksPos  = [[(r+i,c+j) |  i <- [0..2], j <- [0..2]] | r <- [0,3,6], c <- [0,3,6]]

-- | Returns all posible positions for a value to be placed  
getAllPositionForValue :: Sudoku -> SudVal -> [Position]
getAllPositionForValue sud val = concat $ sortOn (length) (getPos' blocksPos) 
-- concat $ sortOn (length) (getPos' blocksPos) MFB
-- reverse $ sortOn (length) (getPos' blockPos) LFB
    where   getPos' :: [[Position]] -> [[Position]]
            getPos' [] = []
            getPos' (s:ss) = (filter (\x -> elem val (getCandidates sud x) && (not (isFilled (valFromPos sud x)))) s) : (getPos' ss)

-- | Order all sudval after a list of ints
sudValOrder :: [Int] -> [SudVal]
sudValOrder list = reverse [snd x |x <-(sortOn fst combine)]
-- reverse [snd x |x <-(sortOn fst combine)] MFN
-- [snd x |x <-(sortOn fst combine)] LFN
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
    {-SingleCandidate       -} if information !! 6 == 8 then 1 else 30 - 2 * information !! 6,
    {-SingelPositionRow     -} if information !! 1 == 8 || information !! 3 == 8 then 2 else 21 - information !! 3,
    {-SinglePositionColumn  -} if information !! 2 == 8 || information !! 4 == 8 then 2 else 21 - information !! 4,
    {-SinglePositionBlock   -} if information !! 5 == 8 || information !! 0 == 8 then 0 else 20 - information !! 5,
    {-NakedPairRow          -} if information !! 1 == 7 || information !! 4 == 7 then 15 else 40 - (information !! 3) - (information !! 6),
    {-NakedPairColumn       -} if information !! 1 == 7 || information !! 4 == 7 then 15 else 40 - (information !! 4) - (information !! 6),
    {-NakedPairBlock        -} if information !! 1 == 7 || information !! 5 == 7 then 15 else 40 - (information !! 5) - (information !! 6),
    {-HiddenPairBlock       -} if information !! 1 == 7 || information !! 4 == 7 then 20 else 50 - (information !! 3) - (information !! 6) + (information !! 6),
    {-HiddenPairRow         -} if information !! 1 == 7 || information !! 4 == 7 then 20 else 50 - (information !! 4) - (information !! 6) + (information !! 6),
    {-HiddenPairColumn      -} if information !! 1 == 7 || information !! 5 == 7 then 20 else 50 - (information !! 5) - (information !! 6) + (information !! 6),
     {-CandidateLine         -} 5000000,
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
        maxFilledNumber = maximum $ (filter (\x-> x /= 9) ((findOccuranceOfNumbers sud)) ++ [0] )
        maxRowsWNotes = mostFilledSectionWNotes sud
        maxColsWNotes = mostFilledSectionWNotes $ columns sud
        maxBlocksWNotes = mostFilledSectionWNotes $ blocks sud


-- | Returns how filled the most non full section is 
mostFilledSection :: [Section] -> Int
mostFilledSection sections = maximum $ filter (\x-> x /= 9) (filledInSections sections) ++ [0]

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
mostFilledSectionWNotes sections = maximum $ filter (\x-> x /= 9) (filledInSectionsWNotes sections) ++ [0]

-- | Takes a list of section and return how filled in each section is counting notes
filledInSectionsWNotes :: [Section] -> [Int]
filledInSectionsWNotes sections = (map filledSectionWNotes sections) ++ [0]

-- | Returns how filled a section is counting notes as filled
filledSectionWNotes :: Section -> Int
filledSectionWNotes [] = 0
filledSectionWNotes ((Empty):sp) = 0 + filledSection sp
filledSectionWNotes (_:sp) = 1 + filledSection sp
