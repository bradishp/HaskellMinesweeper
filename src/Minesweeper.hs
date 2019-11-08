module Minesweeper  (
    Grid (..),
    Row (..),
    Square (..),
    Action (..),
    numberOfMines,
    heightOfMap,
    widthOfMap,
    createGrid,
    updateGrid,
    mineTheField,
    performMoves,
    queueActions, 
    clearSquare,
    placeMine,
    revealTile,
    toggleFlag,
    getSquare,
    targetRowDistance,
    getSurrounding,
    genRandomPairs
) where

import System.Random
import Data.Monoid ((<>))
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics

data Grid = Grid { prevRows :: [Row], nextRows :: [Row], currRowIndex :: Int, numLeft :: Int, numFlagged :: Int }    --Zipper style lists, allows us to easily go backwards and forwards through the list

data Row = Row { prevCols :: [Square], nextCols :: [Square], currColIndex :: Int }

data Square = Clear Int (Int, Int)
            | Hidden Int Bool (Int, Int)
            | Mine Bool (Int, Int)
    deriving Generic

data Action = Action (Square->Square) (Int, Int)

newtype NoQuotes = NoQuotes String      --Taken from https://www.reddit.com/r/haskell/comments/blxveo/print_out_string_without_double_quotes/
instance Show NoQuotes where show (NoQuotes str) = str

instance Show Grid where
    show ( Grid [] [] _ _ _ ) = ""
    show ( Grid [] (x:xs) currRow numLeft numFlagged ) = show x ++ "\n" ++ ( show (Grid [] xs currRow numLeft numFlagged) )
    show grid = show $ startOfGrid grid --Make sure we display the rows in the correct order

instance Eq Grid where
    (==) ( Grid _ _ _ numLeft1 numFlagged1 ) ( Grid _ _ _ numLeft2 numFlagged2 ) = numLeft1 == numLeft2 && numFlagged1 == numFlagged2 

instance Show Row where
    show ( Row [] nextCols _ ) = show nextCols
    show row = show $ startOfRow row --Make sure we display the columns in the correct order

instance Show Square where
    show (Clear n _) = show n --Revealed square
    show (Hidden _ True _) = show (NoQuotes "F") --Flagged square
    show (Mine False _) = show (NoQuotes "X") --Mine only for debugging
    show (Mine True _) = show (NoQuotes "F") --Flagged square
    show _ = show (NoQuotes " ")    --Unknown may be a bomb or empty

--Declare Constants
numberOfMines :: Int
numberOfMines = 10
heightOfMap :: Int
heightOfMap = 9
widthOfMap :: Int
widthOfMap = 9  
        
--Create the grid
createGrid :: Int -> Grid
createGrid squaresToClear = (Grid  [] (genGrid 0) 0 squaresToClear 0)
    where genGrid n | n == heightOfMap = []
                    | otherwise = (Row [] (genRow n 0) 0):(genGrid (n+1))

genRow :: Int -> Int -> [Square]
genRow row col | col == widthOfMap = []
               | otherwise = ( Hidden 0 False (row, col) ):(genRow row (col+1) )

--Perform a set of moves on the grid
updateGrid :: Grid -> Int -> Action-> (Grid, Square)
updateGrid (Grid prevRows (currRow:nextRows) currRowIndex numLeft numFlagged) 0 action = ((Grid prevRows (newRow:nextRows) currRowIndex numLeft numFlagged), newSq)
    where (newRow, newSq) = updateRow currRow (targetColDistance currRow action) action
updateGrid (Grid prevRows (currRow:nextRows) currRowIndex numLeft numFlagged) n action | n < 0 = updateGrid (Grid (currRow:prevRows) nextRows (currRowIndex+1) numLeft numFlagged) (n+1) action
updateGrid (Grid (lastRow:prevRows) nextRows currRowIndex numLeft numFlagged) n action | n > 0 = updateGrid (Grid prevRows (lastRow:nextRows) (currRowIndex-1) numLeft numFlagged) (n-1) action

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
clearSquare :: Grid -> (Int, Int) -> Grid
clearSquare grid coords = case oldSquare of (Clear _ _) -> grid --Tile has already been cleared
                                            _ -> case newSquare of (Mine _ _) -> error ("You landed on a mine and have lost\n" ++ (show grid))
                                                                   _ -> cascadeClear newGrid [coords]   
                                               --Need to clear first tile twice to make sure proper checks are done
    where (_, oldSquare) = updateGrid grid distance (Action getSquare coords)
          action = (Action revealTile coords)
          distance = (targetRowDistance grid action)
          (newGrid, newSquare) = updateGrid grid distance action

cascadeClear :: Grid -> [(Int, Int)] -> Grid
cascadeClear grid [] = grid
cascadeClear grid (coords:rest) = case newSquare of (Clear 0 _) -> isValidClear newGrid (rest ++ (getSurrounding coords))  --Infinite recursion
                                                    (Clear _ _) -> isValidClear newGrid rest 
                                                    _ -> error "Tile isn't revealed after it has been cleared!" --This case should never happen
    where action = (Action revealTile coords)
          ((Grid prevRows nextRows currRowIndex numLeft numFlagged), newSquare) = updateGrid grid (targetRowDistance grid action) action
          newGrid = (Grid prevRows nextRows currRowIndex (numLeft-1) numFlagged)

--Iterates through the list until it finds the next square which needs to be cleared
isValidClear :: Grid -> [(Int, Int)] -> Grid
isValidClear grid [] = grid
isValidClear grid (coords:rest) = case square of (Hidden _ _ _) -> cascadeClear grid (coords:rest)
                                                 _ -> isValidClear grid rest
    where (_, square) = updateGrid grid (targetRowDistance grid action) action
          action = (Action getSquare coords)
          
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
startOfGrid (Grid prevRows nextRows index numLeft numFlagged) = Grid [] (startOfList prevRows nextRows) index numLeft numFlagged

startOfList :: [a] -> [a] -> [a]
startOfList [] ys = ys
startOfList (x:xs) ys = startOfList xs (x:ys)

--Functions for navigating to a certain square
targetRowDistance :: Grid -> Action -> Int
targetRowDistance (Grid _ _ currRowIndex _ _) (Action _ (targRow, _)) = currRowIndex - targRow

targetColDistance :: Row -> Action -> Int
targetColDistance (Row _ _ currColIndex) (Action _ (_, targCol)) = currColIndex - targCol

--Helper functions
inRange :: (Int, Int) -> Bool
inRange (row, col) = row >= 0 && row < heightOfMap && col >= 0 && col < widthOfMap                     

getSurrounding :: (Int, Int) -> [(Int, Int)]
getSurrounding (row, col) = filter inRange [(row-1, col-1), (row-1, col), (row-1, col+1), (row, col-1), (row, col+1), (row+1, col-1), (row+1, col), (row+1, col+1)]

getSquareCoords :: Square -> (Int, Int)
getSquareCoords (Clear _ coords) = coords
getSquareCoords (Hidden _ _ coords) = coords
getSquareCoords (Mine _ coords) = coords

genRandomPairs :: StdGen -> Int -> [(Int, Int)] -> ([(Int, Int)], StdGen)
genRandomPairs newGen 0 list = (list, newGen)
genRandomPairs g n list = if elem (row, col) list then genRandomPairs g3 n list else genRandomPairs g3 (n-1) ((row, col):list) --Make sure the tile doesn't already have a mine in it
    where (row, g2) = randomR (0, heightOfMap-1) g
          (col, g3) = randomR (0, widthOfMap-1) g2    
            