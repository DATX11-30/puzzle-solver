module SudokuLogic where

import Sudoku
import Sudokus
import Data.List
import Control.Applicative (Alternative(empty))
import Debug.Trace (trace)
import Data.Type.Coercion (trans)
import Data.Maybe
import Test.QuickCheck hiding ((===))


type Lemma = Sudoku -> Position -> Bool

-- | A sorted list of all the lemmas avalible sorted in a ~smart~ order	 
lemmas :: [Lemma]
lemmas = [lemmaLastCellInBlock,
          lemmaLastCellInColumn,
          lemmaLastCellInBlock]


-- | A lemma that checks if a value is the last in a block
lemmaLastCellInBlock :: Sudoku -> Position -> Bool
lemmaLastCellInBlock s pos = lastFreeCellInSection s pos (blockFromPos s pos)

-- | A lemma that checks if a value is the last in a row
lemmaLastCellInRow :: Sudoku -> Position -> Bool
lemmaLastCellInRow s pos = lastFreeCellInSection s pos (rowFromPos s pos)

-- | A lemma that checks if a value is the last in a column
lemmaLastCellInColumn :: Sudoku -> Position -> Bool
lemmaLastCellInColumn s pos = lastFreeCellInSection s pos (colFromPos s pos)

-- | if a section only has one empty cell, then that cell must be filled with the only possible value
lastFreeCellInSection :: Sudoku -> Position -> Section -> Bool
lastFreeCellInSection s pos sec = oneCellInSection s sec && not (isFilled $ valFromPos s pos)

-- | A lemma that checks if a value is the only possible value for a cell
lemmaSingleCandidate :: Sudoku -> Position -> Bool
lemmaSingleCandidate sud pos = case candidates of
                                [x] -> True
                                _ -> False
    where
        candidates = getCandidates sud pos

-- | A lemma that checks if a value is the only possible value for a cell in a block
lemmaSinglePositionBlock :: Sudoku -> Position -> Bool
lemmaSinglePositionBlock sud pos = singlePositionSection sud pos (blockPositions pos)

-- | A lemma that checks if a value is the only possible value for a cell in a row
lemmaSinglePositionRow :: Sudoku -> Position -> Bool
lemmaSinglePositionRow sud pos = singlePositionSection sud pos (rowPositions pos)

-- | A lemma that checks if a value is the only possible value for a cell in a column
lemmaSinglePositionColumn :: Sudoku -> Position -> Bool
lemmaSinglePositionColumn sud pos = singlePositionSection sud pos (colPositions pos)

lemmaNakedPairRow :: Sudoku -> Position -> Bool
lemmaNakedPairRow sud pos = nakedPairSection sud pos (rowPositions pos)

lemmaNakedPairColumn :: Sudoku -> Position -> Bool
lemmaNakedPairColumn sud pos = nakedPairSection sud pos (colPositions pos)

lemmaNakedPairBlock :: Sudoku -> Position -> Bool
lemmaNakedPairBlock sud pos = nakedPairSection sud pos (blockPositions pos)

lemmaHiddenPairRow :: Sudoku -> Position -> Bool
lemmaHiddenPairRow sud pos = hiddenPairSection sud pos (rowPositions pos)

lemmaHiddenPairColumn :: Sudoku -> Position -> Bool
lemmaHiddenPairColumn sud pos = hiddenPairSection sud pos (colPositions pos)

lemmaHiddenPairBlock :: Sudoku -> Position -> Bool
lemmaHiddenPairBlock sud pos = hiddenPairSection sud pos (blockPositions pos)

nakedPairSection :: Sudoku -> Position -> [Position] -> Bool
nakedPairSection sud pos sec = case getNakedPairInSection sud pos sec of
                                    (_, []) -> False
                                    (p, vs)-> not (valFromPos sud pos == Note (map Candidate vs)
                                              && valFromPos sud p == Note (map Candidate vs))

hiddenPairSection :: Sudoku -> Position -> [Position] -> Bool
hiddenPairSection sud pos sec = case getHiddenPairInSection sud pos sec of
                                    (_, []) -> False
                                    (p, vs) -> not (valFromPos sud pos == Note (map Candidate vs)
                                              && valFromPos sud p == Note (map Candidate vs))


getNakedPairInSection :: Sudoku -> Position -> [Position] -> (Position, [SudVal])
getNakedPairInSection sud pos sec =  case p of
                                        [] -> (pos, [])
                                        _ -> (head p, posCandidates)
    where
        p = nakedPairs \\ [pos]
        posCandidates = getCandidates sud pos
        pairs = filter (\x -> length (getCandidates sud x) == 2 && not (isFilled (valFromPos sud x))) sec
        nakedPairs = filter (\x -> posCandidates === getCandidates sud x) pairs

getHiddenPairInSection :: Sudoku -> Position -> [Position] -> (Position, [SudVal])
getHiddenPairInSection sud pos sec = case m of
                                [p] -> (p, getMathcingCandidates sud pos p)
                                _ -> (pos, [])
    where
        s = filter (\x -> not (isFilled $ valFromPos sud x)) sec \\ [pos]
        m = filter (\x -> length ( getMathcingCandidates sud pos x) == 2
            &&  all ( `notElem` concatMap (getCandidates sud) (s \\ [x])) (getMathcingCandidates sud pos x)) s


getMathcingCandidates :: Sudoku -> Position -> Position -> [SudVal]
getMathcingCandidates sud p1 p2 = p1Cand `intersect` p2Cand
    where
        p1Cand = getCandidates sud p1
        p2Cand = getCandidates sud p2
-- | 
lemmaCandidateLine :: Sudoku -> Position -> Bool
lemmaCandidateLine sud pos = case concat lines of
                                [] -> False
                                _ -> True
    where
        candidates = getCandidates sud pos
        lines = map (lineBlock sud pos) candidates

lineBlock :: Sudoku -> Position -> SudVal -> [Line]
lineBlock sud pos val
            | isHorizontal && length candidates > 1 && not (isIncluded Horizontal) = [Horizontal]
            | isVertical && length candidates > 1 && not (isIncluded Vertical) = [Vertical]
            | otherwise = []
  where
      isIncluded dir = case posVal of
                          Note x -> Line dir val `elem` x
                          _ -> False
      posVal = valFromPos sud pos
      blockPos = blockPositions pos
      candidates = cand blockPos
      cand = filter (\ x -> val `elem` getCandidates sud x)
      isHorizontal = all (\ x -> fst x == fst pos) candidates
      isVertical = all (\ x -> snd x == snd pos) candidates

prop_lineBlock :: Bool
prop_lineBlock = lineBlock sud (0,0) Two == [Horizontal] &&
                 lineBlock sud (0,1) Five == [Horizontal] &&
                 lineBlock sud (0,3) Six == [Horizontal]
        where
            sud = fillCell (fillCell (fillCell legalSudoku (0,0) Empty) (0,1) Empty) (0,2) Empty


prop_singlePosition_section :: Position -> [SudVal]
prop_singlePosition_section pos = getSinglePosition missingTopRowSud pos (blockPositions pos)


-- | A Lemma That checks whether a cell is the only position in a section that can contain a value
singlePositionSection :: Sudoku -> Position -> [Position] -> Bool
singlePositionSection sud pos secPos = case getSinglePosition sud pos secPos of
                                                [x] -> True
                                                _ -> False


getSinglePosition :: Sudoku -> Position -> [Position] -> [SudVal]
getSinglePosition sud pos secPos  = filter (getOnlyCellInSectionFromValue sud pos secPos) [One .. Nine]

getOnlyCellInSectionFromValue :: Sudoku -> Position -> [Position] -> SudVal -> Bool
getOnlyCellInSectionFromValue sud pos secPos v = not (any (\x -> validPosition sud x v) emptyPos) && validPosition sud pos v
    where
        emptyPos = filter (\x -> not (isFilled $ valFromPos sud x)) secPos \\ [pos]


validPosition :: Sudoku -> Position -> SudVal -> Bool
validPosition sud pos v = v `elem` getCandidates sud pos

valueInSection :: Section -> SudVal -> Bool
valueInSection sec v = Filled v `elem` sec


prop_lastCell :: Position -> Bool
prop_lastCell (row, col) = lemmaLastCellInRow (fillCell illegalSudoku (row, col) Empty) (row, col) &&
                           lemmaLastCellInColumn (fillCell illegalSudoku (row, col) Empty) (row, col) &&
                           lemmaLastCellInBlock (fillCell illegalSudoku (row, col) Empty) (row, col)

-- | Returns all possible candidates 
getCandidates :: Sudoku -> Position -> [SudVal]
getCandidates s pos = candidates
    where
        candidates = [x | x <- [One ..], Filled x `notElem` (nub occupiedVals)]
        row = rowFromPos s pos
        col = colFromPos s pos
        block = blockFromPos s pos
        lineCands = noteLineCandidatesRow (row \\ (intersect row block)) ++ noteLineCandidatesCol (col \\ (intersect col block))
        sections = [row, col, block]
        occupiedVals = values ++ map Filled pairs ++ map Filled lineCands
        pairs =  (nub $ concatMap notePairs sections) \\
                    case valFromPos s pos of
                        Note [Candidate a, Candidate b] -> [a,b]
                        _                               -> []


        values = nub $ concat sections

-- | Testes whether lemma singele candidate is valid
prop_singleCandidate :: Position -> Bool
prop_singleCandidate (row, col) = lemmaSingleCandidate (fillCell legalSudoku (row, col) Empty) (row, col) &&
                                 not (lemmaSingleCandidate legalSudoku (row, col))



--------------------------------------------------------------------------------------------------------
-- Helper functions
-- | If the section includes a line of cellc witch are marked as Candidate line Horizontal, then return those values
noteLineCandidatesRow :: Row -> [SudVal]
noteLineCandidatesRow sec = nub values
    where
        notes = filter isNote sec
        lNotes = concatMap (\(Note x) -> filter (not.isCandidate) x) notes
        values = [v | (Line t v) <- lNotes, t == Horizontal]

prop_lineCandRow :: [SudVal]
prop_lineCandRow = noteLineCandidatesRow (head legalSudoku)
    where
        sud = fillCell (fillCell (fillCell legalSudoku (0,0) Empty) (0,1) Empty) (0,2) Empty

-- | If the section includes a line of cellc witch are marked as Candidate line Vertical, then return those values
noteLineCandidatesCol :: Column -> [SudVal]
noteLineCandidatesCol sec = nub values
    where
        notes = filter isNote sec
        lNotes = concatMap (\(Note x) -> filter (not.isCandidate) x) notes
        values = [v | (Line t v) <- lNotes, t == Vertical]

-- | Returns true if all positions either are fixed at x or fixed at y
secIsLine :: [Position] -> Bool
secIsLine sec = all (\x -> fst x == fst (head sec)) sec

-- | If the section contains two cells witch include a note with the same valus, then return those values
notePairs :: Section -> [SudVal]
notePairs sec = nub pVals
    where
        notes = filter isNote sec
        cNotes = map (\(Note x) -> filter isCandidate x) notes
        pairs = [x | x <- cNotes,  y <- delete x cNotes, x === y]
        pVals = map (\(Candidate x) -> x) (concat pairs)

-- | Returns true if two lists are set-equal
(===) :: Eq a => [a] -> [a] -> Bool
(===) xs ys = null (xs \\ ys)

isCandidate :: Note -> Bool
isCandidate (Candidate _) = True
isCandidate _ = False

isNote :: Value -> Bool
isNote (Note _) = True
isNote _ = False

testSud :: Sudoku
testSud = illegalSudoku

oneCellInSection :: Sudoku -> [Value] -> Bool
oneCellInSection s vs = length (filter isFilled vs) == 8














