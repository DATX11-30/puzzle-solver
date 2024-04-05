module Solver where

import Sudoku
import SudokuLogic
import Sudokus
import Optimizer
import Data.List
data Step =     LastFreeCellBlock Position      |
                LastFreeCellRow Position        |
                LastFreeCellCollumn Position    |
                SingleCandidate Position        |
                SinglePositionRow Position      |
                SinglePositionColumn Position   |
                SinglePositionBlock Position    |
                CandidateLine Position          |
                NakedPairRow Position           |
                NakedPairColumn Position        |
                NakedPairBlock Position         |
                HiddenPairRow Position          |
                HiddenPairColumn Position       |
                HiddenPairBlock Position        |
                NOAVAILABLESTEPS
    deriving (Eq,Show) -- | Then add more step types

type Solution = [Step]

optimSteps :: Sudoku -> [Position -> Step]
optimSteps sud = sudStepOrder $ zip (stepWeight sud) steps

steps :: [Position -> Step]
steps = [--LastFreeCellBlock, LastFreeCellRow, LastFreeCellCollumn, 
        SingleCandidate,
        SinglePositionRow, SinglePositionColumn, SinglePositionBlock
        ,NakedPairBlock, NakedPairRow, NakedPairColumn
        ,HiddenPairBlock, HiddenPairRow, HiddenPairColumn
        ,CandidateLine
        ]

solve :: Sudoku -> Sudoku
solve sud = applySolution sud (fst (generateSolution sud 0))

optimiseResult :: Sudoku -> (Solution, Int)
optimiseResult sud = (generateSolution sud 0)

-- | A solver for sudoku
applySolution :: Sudoku -> Solution -> Sudoku
applySolution = foldl placeValueFromStep

-- | Generates a Solution
generateSolution :: Sudoku -> Int -> (Solution,Int)
generateSolution sud count = case fst next of
                            NOAVAILABLESTEPS -> ([],count)
                            _ -> (fst next : fst res ,snd res) 
    where
        next = nextStep sud (optimSteps sud) 0 --steps -- (optimSteps sud)
        res = generateSolution (placeValueFromStep sud (fst next)) (count + snd next)

placeValueFromStep :: Sudoku -> Step -> Sudoku
placeValueFromStep sud (LastFreeCellBlock p)    = fillCell sud p (valueFromLFCB sud p)
placeValueFromStep sud (LastFreeCellRow p)      = fillCell sud p (valueFromLFCR sud p)
placeValueFromStep sud (LastFreeCellCollumn p)  = fillCell sud p (valueFromLFCC sud p)
placeValueFromStep sud (SingleCandidate p)      = fillCell sud p (valueFromSC sud p)
placeValueFromStep sud (SinglePositionRow p)    = fillCell sud p (valueFromSPR sud p)
placeValueFromStep sud (SinglePositionColumn p) = fillCell sud p (valueFromSPC sud p)
placeValueFromStep sud (SinglePositionBlock p) = fillCell sud p (valueFromSPB sud p)
placeValueFromStep sud (CandidateLine p) = fillCell sud p (valueFromCL sud p)
placeValueFromStep sud (NakedPairRow p) = valueFromNPR sud p
placeValueFromStep sud (NakedPairColumn p) = valueFromNPC sud p
placeValueFromStep sud (NakedPairBlock p) = valueFromNPB sud p
placeValueFromStep sud (HiddenPairRow p) = valueFromHPR sud p
placeValueFromStep sud (HiddenPairColumn p) = valueFromHPC sud p
placeValueFromStep sud (HiddenPairBlock p) = valueFromHPB sud p
placeValueFromStep sud _ = error "Not implemeted this lemma yet :("

-- | Hidden pair row
valueFromHPR :: Sudoku -> Position -> Sudoku
valueFromHPR sud pos = valueFromHPSection sud pos (rowPositions pos)

-- | Hidden pair column
valueFromHPC :: Sudoku -> Position -> Sudoku
valueFromHPC sud pos = valueFromHPSection sud pos (colPositions pos)

-- | Hidden pair block
valueFromHPB :: Sudoku -> Position -> Sudoku
valueFromHPB sud pos = valueFromHPSection sud pos (blockPositions pos)

-- | Returns a soduku with both values from a hidden pair filled in i a section
valueFromHPSection :: Sudoku -> Position -> [Position] -> Sudoku
valueFromHPSection sud pos secPos = case getHiddenPairInSection sud pos secPos of
                                        (p, [v1, v2]) -> fillCell
                                                        (fillCell sud p (Note [Candidate v1, Candidate v2]))
                                                        pos (Note [Candidate v1, Candidate v2])
                                        _ -> error "Not a naked pair"

-- | Returns a soduku with both values from a naked pair filled in i a row
valueFromNPR :: Sudoku -> Position -> Sudoku
valueFromNPR sud pos = valueFromNPSection sud pos (rowPositions pos)

-- | Returns a soduku with both values from a naked pair filled in i a column
valueFromNPC :: Sudoku -> Position -> Sudoku
valueFromNPC sud pos = valueFromNPSection sud pos (colPositions pos)

-- | Returns a soduku with both values from a naked pair filled in i a block
valueFromNPB :: Sudoku -> Position -> Sudoku
valueFromNPB sud pos = valueFromNPSection sud pos (blockPositions pos)

-- | Returns a soduku with both values from a naked pair filled in i a section
valueFromNPSection :: Sudoku -> Position -> [Position] -> Sudoku
valueFromNPSection sud pos secPos = case getNakedPairInSection sud pos secPos of
                                        (p, v1:v2:[]) -> fillCell
                                                        (fillCell sud p (Note [Candidate v1, Candidate v2]))
                                                        pos (Note [Candidate v1, Candidate v2])
                                        _ -> error "Not a naked pair"

-- | Returns the value of a single position in a row
valueFromSPR :: Sudoku -> Position -> Value
valueFromSPR sud pos = valueFromSPSection sud pos (rowPositions pos)

-- | Returns the value of a single position in a column
valueFromSPC :: Sudoku -> Position -> Value
valueFromSPC sud pos = valueFromSPSection sud pos (colPositions pos)

-- | Returns the value of a single position in a block
valueFromSPB :: Sudoku -> Position -> Value
valueFromSPB sud pos = valueFromSPSection sud pos (blockPositions pos)

-- | Returns the value of a single position in a section
valueFromSPSection :: Sudoku -> Position -> [Position] -> Value
valueFromSPSection sud pos secPos = case getSinglePosition sud pos secPos of
                                        [x] -> Filled x
                                        _ -> error "Not a single position"

-- | Returns the value of a single candidate in a cell
valueFromSC :: Sudoku -> Position -> Value
valueFromSC sud pos = case candidates of
                        [x] -> Filled x
                        _ -> error "Not a single candidate"
    where
        candidates = getCandidates sud pos

-- | Last free cell row
valueFromLFCR :: Sudoku -> Position -> Value
valueFromLFCR sud pos = valueFromLFCSection (rowFromPos sud pos)

-- | Last free cell collumn
valueFromLFCC :: Sudoku -> Position -> Value
valueFromLFCC sud pos = valueFromLFCSection (colFromPos sud pos)

-- | Last free cell block
valueFromLFCB :: Sudoku -> Position -> Value
valueFromLFCB sud pos = valueFromLFCSection (blockFromPos sud pos)

-- | Returns the value of the last free cell in a section
valueFromLFCSection :: Section -> Value
valueFromLFCSection sec = case list' of
                            [x] -> x
                            _ -> error "Not a Section does not work with lemma LFCS"
    where
        list' = [Filled x | x <- [One ..]] \\ sec

-- | Returns the value of a candidate line
valueFromCL :: Sudoku -> Position -> Value
valueFromCL sud pos = case filter (\(l,v) -> l /= []) lines of
                        as -> Note $ map (\([l],v) -> Line l v) as
                        -- _ -> error "Not a single candidate" -- Overlapping pattern, might need to change this if it is ia problem
    where
        candidates = getCandidates sud pos
        lines = [(lineBlock sud pos l, l) | l <- candidates, lineBlock sud pos l /= []]




-- | next step in solving the sudoku
nextStep :: Sudoku -> [Position -> Step] -> Int -> (Step,Int)
nextStep sud [] count = (NOAVAILABLESTEPS,count)
nextStep sud (sf:sfs) count = case fst res of
                            NOAVAILABLESTEPS -> nextStep sud sfs (count + snd res)
                            x -> res
        where
                res = tryStepsOnEmpty sud sf count
      
-- | Applies tryStepOnPositions to all empty positions in a given sudoku
tryStepsOnEmpty :: Sudoku -> (Position -> Step) -> Int -> (Step,Int)
tryStepsOnEmpty sud sf count= tryStepOnPositions sud sf (finalOrderOfPosition sud) count  --(emptyPositions sud) (finalOrderOfPosition sud)

-- | Returns all empty positions in a given sudoku
emptyPositions :: Sudoku -> [Position]
emptyPositions sud = [(r,c) | r <- [0..8], c <- [0..8], not (isFilled (valFromPos sud (r,c)))]

propOrderPos sud = (finalOrderOfPosition sud \\ emptyPositions sud)

--prop_getAllPos sud = all (\x -> elem x (emptyPositions sud)) (concat $ getAllPositionForValue sud)

-- | Tries to apply a lemma/step to all position, 
tryStepOnPositions :: Sudoku -> (Position -> Step) -> [Position] -> Int -> (Step,Int)
tryStepOnPositions sud _ [] count       = (NOAVAILABLESTEPS,count)
tryStepOnPositions sud sf (p:ps) count  | testStep sud (sf p) = (sf p,count)
                                        | otherwise = tryStepOnPositions sud sf ps (count+1)

-- | Tests if a step is valid for the given sudoku
testStep :: Sudoku -> Step -> Bool
testStep sud (LastFreeCellBlock p) = lemmaLastCellInBlock sud p
testStep sud (LastFreeCellRow p) = lemmaLastCellInRow sud p
testStep sud (LastFreeCellCollumn p) = lemmaLastCellInColumn sud p
testStep sud (SingleCandidate p) = lemmaSingleCandidate sud p
testStep sud (SinglePositionRow p) = lemmaSinglePositionRow sud p
testStep sud (SinglePositionColumn p) = lemmaSinglePositionColumn sud p
testStep sud (SinglePositionBlock p) = lemmaSinglePositionBlock sud p
testStep sud (CandidateLine p) = lemmaCandidateLine sud p
testStep sud (NakedPairRow p) = lemmaNakedPairRow sud p
testStep sud (NakedPairColumn p) = lemmaNakedPairColumn sud p
testStep sud (NakedPairBlock p) = lemmaNakedPairBlock sud p
testStep sud (HiddenPairRow p) = lemmaHiddenPairRow sud p
testStep sud (HiddenPairColumn p) = lemmaHiddenPairColumn sud p
testStep sud (HiddenPairBlock p) = lemmaHiddenPairBlock sud p
testStep sud _ = error "lemma not implemented"



isSolved :: Sudoku -> Bool
isSolved sud = all isFilled (concat sud)
