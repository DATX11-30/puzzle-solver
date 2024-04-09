import Data.List
import IO
import Logic
import Optimizer
import Solver
import Sudoku
import SudokuLogic
import Sudokus
import System.Directory (doesFileExist)
import System.Environment

main :: IO ()
main = do
  args <- getArgs

  if length args == 1
    then do
      let filePath :: FilePath = head args
      let solFilePath = take (length filePath - 4) filePath ++ "_sol.txt"
      fileExists <- doesFileExist solFilePath
      if fileExists
        then do
          content <- readFile solFilePath
          print content
        else do
          sud <- readSudoku filePath
          let solution = fst (generateSolution sud 0)
          writeFile solFilePath (show solution ++ "\n")
          print (show solution)
          applyStepAndWrite sud solution solFilePath
    else
      if length args == 2
        then do
          let sudString :: String = args !! 1
          let sud = parseSudoku sudString
          let solution = fst (generateSolution sud 0)
          print (show solution)
        else putStrLn "Usage: sudoku <sudoku_file_path> <sudoku_string>"
  return ()


applyStepAndWrite :: Sudoku -> Solution -> FilePath -> IO ()
applyStepAndWrite sud [] _ = return ()
applyStepAndWrite sud (x:xs) filePath = do
  let sud' = placeValueFromStep sud x
  let s = deParseSudoku sud'
  appendFile filePath (s ++ "\n")
  applyStepAndWrite sud' xs filePath
  return ()
 
