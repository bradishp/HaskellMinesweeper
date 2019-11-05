module Main where

import Lib
import System.Random
import System.IO

data GameStatus = Ongoing 
                  | Victory 
                  | GameOver

data Grid = Grid { prevRows :: [Row], nextRows :: [Row], currRowIndex :: Int }    --Zipper list, allows us to easily go backwards and forwards through the list
                                                
data Row = Row { prevCols :: [Square], nextCols :: [Square], currColIndex :: Int }

data Square = Clear Int (Int, Int)
            | Hidden Int Bool (Int, Int)
            | Mine Bool (Int, Int)


data Action = Action (Square->Square) (Int, Int)

newtype NoQuotes = NoQuotes String      --Taken from https://www.reddit.com/r/haskell/comments/blxveo/print_out_string_without_double_quotes/
instance Show NoQuotes where show (NoQuotes str) = str

instance Show Grid where
    show ( Grid [] [] _ ) = ""
    show ( Grid [] (x:xs) currRow ) = show x ++ "\n" ++ ( show (Grid [] xs currRow) )
    show grid = show $ startOfGrid grid --Make sure we start displaying the rows at the begining

instance Show Row where
    show ( Row [] nextCols _ ) = show nextCols
    show row = show $ startOfRow row

instance Show Square where
    show (Clear n _) = show n --Revealed square
    show (Hidden _ True _) = show (NoQuotes "F") --Flagged square
    show (Mine False _) = show (NoQuotes "X") --Mine only for debugging
    show (Mine True _) = show (NoQuotes "F") --Flagged square
    show _ = show (NoQuotes " ")    --Unknown

--Declare Constants
numberOfMines :: Int
numberOfMines = 10
lengthOfMap :: Int
lengthOfMap = 8   --Map is always a square

--Setup the game
main :: IO ()
main = do
    generator <- newStdGen
    let randomPairs = genRandomPairs generator numberOfMines [] --Pass in an empty list which we can recursively add the pairs to so long as they haven't already been added
    let grid = createGrid
    let minedGrid = mineTheField grid randomPairs
    let squaresToClear = (lengthOfMap * lengthOfMap) - numberOfMines 
    print minedGrid
    runTurn minedGrid squaresToClear

--Run until the game is finished
runTurn :: Grid -> Int -> IO ()
runTurn grid 0 = do
    print grid
    print "Congratulations. The mines can now be cleared. You have won!"
runTurn grid left = do    
    print "Select a row"
    rowString <- getLine     
    let row = read rowString :: Int
    print "Select a column"
    colString <- getLine     
    let col = read colString :: Int
    let (updatedGrid, updatedLeft) = selectTile grid (row, col) left

 {- print "Now place a flag"
    print "Select a row"
    rowString2 <- getLine     
    let row2 = read rowString2 :: Int
    print "Select a column"
    colString2 <- getLine     
    let col2 = read colString2 :: Int
    -}

    print updatedGrid
    print $ show updatedLeft
    runTurn updatedGrid updatedLeft

--Create the grid
createGrid :: Grid
createGrid = (Grid  [] (genGrid 0) 0)
    where genGrid n | n == lengthOfMap = []
                    | otherwise = (Row [] (genRow n 0) 0):(genGrid (n+1))

genRow :: Int -> Int -> [Square]
genRow row col | col == lengthOfMap = []
               | otherwise = ( Hidden 0 False (row, col) ):(genRow row (col+1) )

--Perform a set of moves on the grid
updateGrid :: Grid -> Int -> Action-> (Grid, Square)
updateGrid (Grid prevRows (currRow:nextRows) currRowIndex) 0 action = ((Grid prevRows (newRow:nextRows) currRowIndex), newSq)
    where (newRow, newSq) = updateRow currRow (targetColDistance currRow action) action
updateGrid (Grid prevRows (currRow:nextRows) currRowIndex) n action | n < 0 = updateGrid (Grid (currRow:prevRows) nextRows (currRowIndex+1)) (n+1) action
updateGrid (Grid (lastRow:prevRows) nextRows currRowIndex) n action | n > 0 = updateGrid (Grid prevRows (lastRow:nextRows) (currRowIndex-1)) (n-1) action

updateRow :: Row -> Int -> Action -> (Row, Square)
updateRow (Row prevSqs ((currSq):nextSqs) currColIndex) 0 (Action op coords) 
    | (getSquareCoords currSq) /= coords = error "Somehow went to the wrong square"
    | (getSquareCoords currSq) == coords = ((Row prevSqs (newSq:nextSqs) currColIndex), newSq)
    where newSq = op currSq
updateRow (Row prevSqs (currSq:nextSqs) currColIndex) n action | n < 0 = updateRow (Row (currSq:prevSqs) nextSqs (currColIndex+1)) (n+1) action
updateRow (Row (lastSq:prevSqs) nextSqs currColIndex) n action | n > 0 = updateRow (Row prevSqs (lastSq:nextSqs) (currColIndex-1)) (n-1) action

--Place the mines on the field
mineTheField :: Grid -> [(Int, Int)] -> Grid
mineTheField grid [] = grid
mineTheField grid (mine:mines) = mineTheField (performMoves grid actions) mines
    where actions = (Action placeMine mine):(queueActions incrAdjMineCount (getSurrounding mine))   --place the mine and increment the surrounding tiles

--TODO refactor + rename
performMoves :: Grid -> [Action] -> Grid
performMoves grid [] = grid
performMoves grid (action:actions) = performMoves (fst (updateGrid grid (targetRowDistance grid action) action)) actions

queueActions :: (Square->Square) -> [(Int, Int)] -> [Action]
queueActions _ [] = []
queueActions op (x:xs) = (Action op x):(queueActions op xs)

--Reveal a tile
selectTile :: Grid -> (Int, Int) -> Int -> (Grid, Int)
selectTile grid (row, col) left = case newSquare of (Mine _ _) -> error ("You landed on a mine and have lost\n" ++ (show grid))
                                                    _ -> clearTiles newGrid [(row, col)] left   --Need to clear first tile twice to make sure proper checks are done
    where action = (Action revealTile (row, col))
          (newGrid, newSquare) = updateGrid grid (targetRowDistance grid action) action

--TODO get left to decrement properly
clearTiles :: Grid -> [(Int, Int)] -> Int -> (Grid, Int)
clearTiles grid [] left = (grid, left)
clearTiles grid ((row, col):rest) left = case newSquare of (Mine _ _) -> clearTiles grid rest (left)    --This case shouldn't happen
                                                           (Clear 0 _) -> clearTiles newGrid (rest ++ ( cascadeReveal  newGrid (getSurrounding (row, col)) )) (left-1)   --Infinite recursion
                                                           (Clear _ _) -> clearTiles newGrid rest (left-1)
    where action = (Action revealTile (row, col))
          (newGrid, newSquare) = updateGrid grid (targetRowDistance grid action) action


cascadeReveal :: Grid -> [(Int, Int)] -> [(Int, Int)]
cascadeReveal grid [] = []
cascadeReveal grid ((row, col):rest) = case square of (Hidden _ _ _) -> ((row, col):(cascadeReveal grid rest))
                                                      _ -> cascadeReveal grid rest
    where (_, square) = updateGrid grid (targetRowDistance grid action) action
          action = (Action getSquare (row, col))
          
--Various Actions you can perform on square
placeMine :: Square -> Square
placeMine (Hidden _ flag coords) = Mine flag coords
placeMine _ = error "Can't place a mine there"

revealTile :: Square -> Square
revealTile (Clear n coords) = Clear n coords                                
revealTile (Hidden n _ coords) = Clear n coords
revealTile (Mine flag coords) = Mine flag coords

--Place or remove a flag on a tile
toggleFlag :: Square -> Square
toggleFlag (Clear n coords) = Clear n coords
toggleFlag (Hidden n flag coords) = Hidden n (not flag) coords
toggleFlag (Mine flag coords) = Mine (not flag) coords

--Increment the number of adjacent mines
incrAdjMineCount :: Square -> Square
incrAdjMineCount (Clear n coords) = Clear (n+1) coords
incrAdjMineCount (Hidden n flag coords) = Hidden (n+1) flag coords
incrAdjMineCount (Mine flag coords) = Mine flag coords

getSquare :: Square -> Square
getSquare sq = sq

--Functions for returning to the begining of a row or the grid
startOfRow :: Row -> Row
startOfRow (Row prevCols nextCols index ) = Row [] (startOfList prevCols nextCols) index

startOfGrid :: Grid -> Grid
startOfGrid (Grid prevRows nextRows index) = Grid [] (startOfList prevRows nextRows) index

startOfList :: [a] -> [a] -> [a]
startOfList [] ys = ys
startOfList (x:xs) ys = startOfList xs (x:ys)

--Functions for navigating to a certain square
targetRowDistance :: Grid -> Action -> Int
targetRowDistance (Grid _ _ currRowIndex) (Action _ (targRow, _)) = currRowIndex - targRow

targetColDistance :: Row -> Action -> Int
targetColDistance (Row _ _ currColIndex) (Action _ (_, targCol)) = currColIndex - targCol

--Helper functions
inRange :: (Int, Int) -> Bool
inRange (row, col) = row >= 0 && row < lengthOfMap && col >= 0 && col < lengthOfMap

genRandomPairs :: StdGen -> Int -> [(Int, Int)] -> [(Int, Int)]
genRandomPairs _ 0 list = list
genRandomPairs g n list = if elem (row, col) list then genRandomPairs g3 n list else genRandomPairs g3 (n-1) ((row, col):list) --Make sure the tile doesn't already have a mine in it
    where (row, g2) = randomR (0, lengthOfMap-1) g
          (col, g3) = randomR (0, lengthOfMap-1) g2                           

getSurrounding :: (Int, Int) -> [(Int, Int)]
getSurrounding (row, col) = filter inRange [(row-1, col-1), (row-1, col), (row-1, col+1), (row, col-1), (row, col+1), (row+1, col-1), (row+1, col), (row+1, col+1)]

getSquareCoords :: Square -> (Int, Int)
getSquareCoords (Clear _ coords) = coords
getSquareCoords (Hidden _ _ coords) = coords
getSquareCoords (Mine _ coords) = coords

--Computer Solver
makeTrivialMoves :: Grid -> Grid
makeTrivialMoves grid = grid
