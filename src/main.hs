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
          let result = generateSolution sud emptySudoku 0
          let solution = fst result
          let amountChecks = snd result
          writeFile solFilePath ("Solution:\n" ++ outputArray (Origin : solution) show ++ "\n" ++ show amountChecks ++ "\n")
          print (show solution)
          appendFile solFilePath "PartialSudokus:\n"
          appendFile solFilePath (deParseSudoku sud ++ "\n")
          applyStepAndWrite sud solution solFilePath
          --writeFile solFilePath (test sud solution amountChecks)

    else
      if length args == 2
        then do
          let sudString :: String = args !! 1
          let sud = parseSudoku sudString
          let result = generateSolution sud emptySudoku 0
          let solution = fst result
          let amountChecks = snd result
          print (show solution)
        else putStrLn "Usage: sudoku <sudoku_file_path> <sudoku_string>"
  return ()

--Can't get it to work
test :: Sudoku -> Solution -> Int -> String
test sud sol checks = "{\n" ++ 
  "  \"solution\": [" ++ outputArray sol show ++ "],\n" ++
  "  \"checks\": " ++ show checks ++ ",\n" ++
  "  \"partialSudokus\": [" ++ outputArray (getPartialSudokus sud sol) deParseSudoku ++"]\n" ++
  "}"

applyStepAndWrite :: Sudoku -> Solution -> FilePath -> IO ()
applyStepAndWrite sud [] _ = return ()
applyStepAndWrite sud (x:xs) filePath = do
  let sud' = placeValueFromStep sud x
  let s = deParseSudoku sud'
  appendFile filePath (s ++ "\n")
  applyStepAndWrite sud' xs filePath
  return ()


getPartialSudokus :: Sudoku -> Solution -> [Sudoku]
getPartialSudokus sud sol = getPartialSudokus' sud sol [sud]

getPartialSudokus' :: Sudoku -> Solution -> [Sudoku] -> [Sudoku]
getPartialSudokus' sud [] partials = partials
getPartialSudokus' sud (x:xs) partials = getPartialSudokus' sud' xs (sud':partials)
  where sud' = placeValueFromStep sud x
 
outputArray:: Show a => [a] -> (a -> String) -> String
outputArray [x] f = "\"" ++ f x ++ "\""
outputArray (x:xs) f = "\"" ++ (f x) ++ "\",\n" ++ outputArray xs f