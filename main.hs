import Data.List

printBoard :: [[Int]] -> IO ()
printBoard sudoku = mapM_ print (sudoku)

validRow :: Int -> [Int] -> Bool
validRow x line = not (x `elem` line)

validColumn :: Int -> [Int] -> Bool
validColumn x column = not (x `elem` column)

validSquare :: Int -> Int -> Int -> [[Int]] -> Bool
validSquare value row col sudoku =
    let row_square = [(row `div` 3 * 3)..(row `div` 3 * 3)+2]
        col_square = [(col `div` 3 * 3)..(col `div` 3 * 3)+2]
    in not (value `elem` [ sudoku !! y !! x | y <- row_square, x <- col_square])

isValid :: Int -> Int -> Int -> [[Int]] -> Bool
isValid value row col sudoku =
    let rowIsValid = validRow value (sudoku !! row) 
        colIsValid = validColumn value (transpose sudoku !! col)
        squareIsValid = validSquare value row col sudoku
    in (rowIsValid && colIsValid && squareIsValid)

sudokuSolve :: [[Int]] -> [Int]
sudokuSolve sudoku = [ value | 
                        value <- [1..9], col <- [0..8], row <- [0..8], 
                        (sudoku !! col !! row == 0) && (isValid value row col sudoku)]

-- for value in (1..9)
--     for col in (0..8)
--         for row in (0..8)
--             if (sudoku[row][col] == 0) && (isValid value row col sudoku)
--                 sudoku[row][col] = value
--                 output.push(value)

main = do
    -- Aqui os valores com 0 são os espaços vazios
    let sudoku = [
                [0,0,0,1,4,0,0,0,0],
                [0,0,0,1,5,0,0,0,0],
                [0,0,0,1,6,0,0,0,0],
                [0,0,0,1,0,0,0,0,0],
                [0,0,0,1,0,0,0,0,0],
                [0,0,0,1,0,0,0,0,0],
                [0,0,0,1,0,0,0,0,0],
                [0,0,0,1,0,0,0,0,0],
                [0,0,0,1,0,0,0,0,0]
            ]    
    print (sudokuSolve sudoku)
    -- print (isValid 3 2 0 sudoku)
    -- printBoard (sudoku)
    


