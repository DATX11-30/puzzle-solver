module IO where
import Solver
import Sudokus
import Sudoku

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