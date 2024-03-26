module IO where
import Solver
import Sudokus
import Sudoku
import SudokuLogic (getCandidates, getHiddenPairInSection)

-- A function to simplify the solut
showSudoku :: Sudoku -> IO ()
showSudoku sud = putStrLn $ showSud sud

showSolvedSudoku :: Sudoku -> IO ()
showSolvedSudoku sud = showSudoku (solve sud)

showSud :: Sudoku -> String
showSud sud = topRow
            ++ "\n" ++ showRow r1 ++ "\9474"
            ++ "\n" ++ showRow r2 ++ "\9474"
            ++ "\n" ++ showRow r3 ++ "\9474"
            ++ "\n" ++ midRow
            ++ "\n" ++ showRow r4 ++ "\9474"
            ++ "\n" ++ showRow r5 ++ "\9474"
            ++ "\n" ++ showRow r6 ++ "\9474"
            ++ "\n" ++ midRow
            ++ "\n" ++ showRow r7 ++ "\9474"
            ++ "\n" ++ showRow r8 ++ "\9474"
            ++ "\n" ++ showRow r9 ++ "\9474"
            ++ "\n" ++ botRow
    where
        topRow = "\9484" ++ "\9472" ++ "\9472" ++ "\9472" ++ "\9472" ++ "\9472" ++ "\9472" ++ "\9472" ++ "\9516"++ "\9472" ++ "\9472" ++ "\9472" ++ "\9472" ++ "\9472" ++ "\9472" ++ "\9472" ++ "\9516"++ "\9472" ++ "\9472" ++ "\9472" ++ "\9472" ++ "\9472" ++ "\9472" ++ "\9472" ++ "\9488"
        midRow = "\9500" ++ "\9472" ++ "\9472" ++ "\9472" ++ "\9472" ++ "\9472" ++ "\9472" ++ "\9472" ++ "\9532"++ "\9472" ++ "\9472" ++ "\9472" ++ "\9472" ++ "\9472" ++ "\9472" ++ "\9472" ++ "\9532"++ "\9472" ++ "\9472" ++ "\9472" ++ "\9472" ++ "\9472" ++ "\9472" ++ "\9472" ++ "\9508"
        botRow = "\9492" ++ "\9472" ++ "\9472" ++ "\9472" ++ "\9472" ++ "\9472" ++ "\9472" ++ "\9472" ++ "\9524"++ "\9472" ++ "\9472" ++ "\9472" ++ "\9472" ++ "\9472" ++ "\9472" ++ "\9472" ++ "\9524"++ "\9472" ++ "\9472" ++ "\9472" ++ "\9472" ++ "\9472" ++ "\9472" ++ "\9472" ++ "\9496"

        (r1:r2:r3:r4:r5:r6:r7:r8:r9:xs) = rows sud
showRow :: [Value] -> String
showRow [] = ""
showRow (x1:x2:x3:xs) = "\9474" ++ " " ++ show x1 ++ " " ++ show x2 ++ " " ++ show x3 ++ " " ++ showRow xs

charToSudVal :: Char -> Value
charToSudVal '1' = Filled One
charToSudVal '2' = Filled Two
charToSudVal '3' = Filled Three
charToSudVal '4' = Filled Four
charToSudVal '5' = Filled Five
charToSudVal '6' = Filled Six
charToSudVal '7' = Filled Seven
charToSudVal '8' = Filled Eight
charToSudVal '9' = Filled Nine
charToSudVal '0' = Empty
charToSudVal _ = error "incorrect value in file"

parseSudoku :: String -> Sudoku
parseSudoku [] = []
parseSudoku xs = parse (take 9 xs) : parseSudoku (drop 9 xs)
        where
                parse :: [Char] -> [Value]
                parse [] = []
                parse (y:ys) = charToSudVal y : parse ys

readSudoku :: FilePath -> IO Sudoku
readSudoku filepath = do
        content <- readFile filepath
        return $ parseSudoku $ take 81 content

showSudokuFromFile :: FilePath -> IO ()
showSudokuFromFile filepath = do
        sud <- readSudoku filepath
        showSudoku sud

showSolvedSudokuFromFile :: FilePath -> IO ()
showSolvedSudokuFromFile filepath = do
        sud <- readSudoku filepath
        showSolvedSudoku sud

generateSolutionFromFile :: FilePath -> IO ()
generateSolutionFromFile filepath = do
        sud <- readSudoku filepath
        print (fst (generateSolution sud 0))

applySolutionFromFile :: FilePath -> [Step] -> IO ()
applySolutionFromFile filepath steps = do
        sud <- readSudoku filepath
        showSudoku (applySolution sud steps)

getCandidatesIO :: FilePath -> Position -> IO [SudVal]
getCandidatesIO filepath pos = do
        sud <- readSudoku filepath
        return $ getCandidates sud pos

testHiddenPairs :: FilePath -> Position -> [Step] -> IO ()
testHiddenPairs filepath pos steps = do
        sud <- readSudoku filepath
        print $ getHiddenPairInSection (applySolution sud steps) pos (blockPositions pos)