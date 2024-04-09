module SudokuLogic where

import Sudoku
import Sudokus
import Data.List
import Control.Applicative (Alternative(empty))
import Debug.Trace (trace)
import Data.Type.Coercion (trans)
import Data.Maybe
--import Test.QuickCheck hiding ((===))


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

-- | Returns the position of the other part of the pair and the value of the pair in the form (Position, [SudVal]) 
-- The corresponding pair is with the inserted position. 
-- If no pair is found, then return the inserted position and an empty list 
getNakedPairInSection :: Sudoku -> Position -> [Position] -> (Position, [SudVal])
getNakedPairInSection sud pos sec =  case p of
                                        [] -> (pos, [])
                                        _ -> if isIncluded then (pos, []) else (head p, posCandidates)
    where
        -- The candidates of the inserted position
        posCandidates = getCandidates sud pos
        -- The nonfilled positions in the section which also only has two candidates
        pairs = filter (\x -> length (getCandidates sud x) == 2 && not (isFilled (valFromPos sud x))) sec
        -- The positions with matching candidates with the inserted position, i.e. the pairs
        nakedPairs = filter (\x -> posCandidates === getCandidates sud x) pairs
        -- The list of pairs with the original position removed.
        p = nakedPairs \\ [pos]

        posVal = valFromPos sud pos
        isIncluded = case posVal of
                        Note x -> Candidate (head posCandidates) `elem` x && Candidate (last posCandidates) `elem` x
                        _ -> False


-- | Returns the position of the other part of the pair and the value of the pair in the form (Position, [SudVal]) 
-- The corresponding pair is with the inserted position. 
-- If no pair is found, then return the inserted position and an empty list 
getHiddenPairInSection :: Sudoku -> Position -> [Position] -> (Position, [SudVal])
getHiddenPairInSection sud pos sec = case m of
                                [(p, cs)] -> if isIncluded cs then (pos, []) else (p, cs)
                                [] -> (pos, [])
                                _ -> error "Multiple hidden pairs"
    where
        -- The candidates of the inserted position
        posCand = getCandidates sud pos
        -- The empty position in the section that are not the inserted position
        s = filter (\x -> not (isFilled $ valFromPos sud x)) sec \\ [pos]
        -- The candidates of the empty positions in the type [([candidates], position)]
        cand = map (\x -> (getCandidates sud x, x)) s

        -- The postions that have mathing candidates with the origianal position but not any other
        a = filter (\(cs, p) -> fv posCand cs (map (getCandidates sud) s)) cand

        -- The positions with pairs i.e. the ones with only two mathing candidates 
        m = map (\(cs, p) -> (p, f posCand cs (map (getCandidates sud) s))) a

        fv :: [SudVal] -> [SudVal] -> [[SudVal]] -> Bool
        fv me other secCand = length (f me other secCand) == 2

        -- | Returns the values that are both in me and other but not in the rest of the section
        f :: [SudVal] -> [SudVal] -> [[SudVal]] -> [SudVal]
        f me other secCand = filter (\x -> x `notElem` concat (secCand \\ [other])) (intersect me other)

        posVal = valFromPos sud pos
        isIncluded cs = case posVal of
                        Note x -> Candidate (head cs) `elem` x && Candidate (last cs) `elem` x
                        _ -> False

-- | Returns true if the posions has a candidate line
lemmaCandidateLine :: Sudoku -> Position -> Bool
lemmaCandidateLine sud pos = case concat lines of
                                [] -> False
                                _ -> True
    where
        candidates = getCandidates sud pos
        lines = map (lineBlock sud pos) candidates

-- | Retuns the Line the position has if it has one
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
        -- Takes all possible candidates and formats them to be SudVals
        candidates = [x | x <- [One ..], Filled x `notElem` nub occupiedVals]

        row = rowFromPos s pos
        col = colFromPos s pos
        block = blockFromPos s pos
        -- 
        lineCands = noteLineCandidatesRow (row \\ intersect row block) ++ noteLineCandidatesCol (col \\ (intersect col block))
        
        sections = [row, col, block]
        
        occupiedVals = values ++ map Filled pairs ++ map Filled lineCands

        pairs =  nub (concatMap notePairs sections) \\ 
                    case valFromPos s pos of
                        Note xs -> getNotePairs xs
                        _       -> []
        values = nub $ concat sections

getNotePairs :: [Note] -> [SudVal]
getNotePairs [] = []
getNotePairs ((Candidate a):xs) = a : getNotePairs xs
getNotePairs (_:xs) = getNotePairs xs




-- | If the section contains two cells witch include a note with the same valus, then return those values
notePairs :: Section -> [SudVal]
notePairs sec = nub pVals
    where
        notes = filter isNote sec
        cNotes = map (\(Note x) -> filter isCandidate x) notes
        pairs = [x | x <- cNotes,  y <- delete x cNotes, x === y]
        pVals = map (\(Candidate x) -> x) (concat pairs)

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



-- | Returns true if two lists are set-equal
(===) :: Eq a => [a] -> [a] -> Bool
(===) xs ys = null (xs \\ ys) && null (ys \\ xs)

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