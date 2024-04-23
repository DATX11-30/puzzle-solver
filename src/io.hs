module IO where
import Solver
import Sudokus
import Sudoku
import SudokuLogic (getCandidates, getHiddenPairInSection)
import System.Directory
import Data.List



-- A function to simplify the solut
showSudoku :: Sudoku -> IO ()
showSudoku sud = putStrLn $ showSud sud

showSolvedSudoku :: Sudoku -> Sudoku-> IO ()
showSolvedSudoku sud sol = showSudoku (solve sud sol)

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


sudValToChar :: Value -> String

sudValToChar (Filled One) = "1"
sudValToChar (Filled Two) = "2"
sudValToChar (Filled Three) = "3"
sudValToChar (Filled Four) = "4"
sudValToChar (Filled Five) = "5"
sudValToChar (Filled Six) = "6"
sudValToChar (Filled Seven) = "7"
sudValToChar (Filled Eight) = "8"
sudValToChar (Filled Nine) = "9"
sudValToChar (Note x) = show x
sudValToChar Empty = "0"


deParseSudoku :: Sudoku -> String
deParseSudoku sud = concatMap deParse sud
        where
                deParse :: [Value] -> String
                deParse [] = []
                deParse (x:xs) = sudValToChar x ++ deParse xs





readSudoku :: FilePath -> IO Sudoku
readSudoku filepath = do
        content <- readFile filepath
        return $ parseSudoku $ take 81 content

readSudNSol :: FilePath -> IO (Sudoku, Sudoku)
readSudNSol filepath = do
        content <- readFile filepath
        let ls = lines content
        let sud = parseSudoku $ head ls
        let sol = parseSudoku $ ls !! 1
        return (sud, sol)

showSudokuFromFile :: FilePath -> IO ()
showSudokuFromFile filepath = do
        sud <- readSudoku filepath
        showSudoku sud

showSolvedSudokuFromFile :: FilePath -> IO ()
showSolvedSudokuFromFile filepath = do
        sud <- readSudoku filepath
        showSolvedSudoku sud emptySudoku

showSudNSolFromFile :: FilePath -> IO ()
showSudNSolFromFile filepath = do
        (sud, sol) <- readSudNSol filepath
        putStrLn "Sudoku:"
        showSudoku sud
        putStrLn "Solution:"
        showSudoku sol

generateSolutionFromFile :: FilePath -> IO ()
generateSolutionFromFile filepath = do
        sud <- readSudoku filepath
        print (fst(generateSolution sud emptySudoku 0))

applySolutionFromFile :: FilePath -> [Step] -> IO ()
applySolutionFromFile filepath steps = do
        sud <- readSudoku filepath
        showSudoku (applySolution sud steps)

getCandidatesIO :: FilePath -> Position -> IO [SudVal]
getCandidatesIO filepath pos = do
        sud <- readSudoku filepath
        return $ getCandidates sud pos


getAllSudokusInDir :: FilePath -> IO [FilePath]
getAllSudokusInDir dir = do
        files <- listDirectory dir
        -- Filter out .DS_Store files
        files <- return $ filter (/= ".DS_Store") files
        -- Filter out all Directories
        files <- return $ filter (\x -> (elem '.' x)) files
        -- Sort alphabetically 
        files <- return $ sortOn (\x -> x) files
        return $ map (\x -> dir ++ "/" ++ x) files

showAllSudokusInDir :: FilePath -> IO ()
showAllSudokusInDir dir = do
        files <- getAllSudokusInDir dir
        mapM_ showSudokuFromFile files

countChecksFromFile :: FilePath -> IO ()
countChecksFromFile filepath = do
        sud <- readSudoku filepath
        print filepath
        let result = (checksCount sud emptySudoku)
        let sud' = applySolution sud (fst result)
        --showSudoku sud
        --showSudoku sud'
        --print (fst result)
        print (isSolved sud')
        print (snd result)

countChecksFromFileToFile :: FilePath -> IO ()
countChecksFromFileToFile filepath = do
        sud <- readSudoku filepath
        --appendFile "./result.txt" (filepath ++ "\n")
        let result = (generateSolution sud emptySudoku 0)
        let sud' = applySolution sud (fst result)
        appendFile "./result.txt" (if isSolved sud' then (show(snd result) ++ "\n") else (show(isSolved sud') ++ "\n"))
        print(filepath)
        return ()

countChecksForAllInDirToFile :: FilePath -> IO ()
countChecksForAllInDirToFile dir = do
        appendFile "./result.txt" dir
        files <- getAllSudokusInDir dir
        print(length files)
        mapM_ countChecksFromFileToFile files

countChecksForAllInDir :: FilePath -> IO ()
countChecksForAllInDir dir = do
        files <- getAllSudokusInDir dir
        mapM_ countChecksFromFile files

testHiddenPairs :: FilePath -> Position -> [Step] -> IO ()
testHiddenPairs filepath pos steps = do
        sud <- readSudoku filepath
        print $ getHiddenPairInSection (applySolution sud steps) pos (blockPositions pos)

