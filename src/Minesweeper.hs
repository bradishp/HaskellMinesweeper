module Minesweeper  (
    Grid (..),
    Row (..),
    DisplayedSquare (..),
    Action (..),
    numberOfMines,
    heightOfMap,
    widthOfMap,
    createBlankGrid,
    createMinedGrid,
    placeFlag,
    lookupSquare,
    clearSquare,
    getSurrounding,
    genRandomCoords,
    getDisplayGrid
) where

import System.Random
import Data.Monoid ((<>))
import Lib

--Zipper style lists, allows us to easily go backwards and forwards through the list
data Grid = Grid { prevRows :: [Row], nextRows :: [Row], currRowIndex :: Int, numLeft :: Int, numFlagged :: Int } 

data Row = Row { prevCols :: [Square], nextCols :: [Square], currColIndex :: Int }

data DisplayedSquare = Visible Int
                    | Unknown Bool

data Square = Clear Int
            | Hidden Int Bool
            | Mine Bool

data Action = Action (Square->Square) (Int, Int)    --Takes the coordinates of a square and a function to run on that square

newtype NoQuotes = NoQuotes String      --Taken from https://www.reddit.com/r/haskell/comments/blxveo/print_out_string_without_double_quotes/
instance Show NoQuotes where show (NoQuotes str) = str

instance Show Grid where
    show ( Grid [] [] _ _ _ ) = ""
    show ( Grid [] (x:xs) currRow numLeft numFlagged ) = show x ++ "\n" ++ ( show (Grid [] xs currRow numLeft numFlagged) )
    show grid = show $ startOfGrid grid --Make sure we display the rows in the correct order

instance Show Row where
    show ( Row [] nextCols _ ) = show nextCols
    show row = show $ startOfRow row --Make sure we display the columns in the correct order

instance Show Square where  --Used for terminal version of the game
    show (Clear n) = show n --Revealed square
    show (Hidden _ True) = show (NoQuotes "F") --Flagged square
    show (Mine False) = show (NoQuotes "X") --Mine only for debugging TODO remove this
    show (Mine True) = show (NoQuotes "F") --Flagged square
    show _ = show (NoQuotes " ")    --Unknown may be a bomb or empty

--Declare Constants
numberOfMines :: Int
numberOfMines = 10 --B=10 I=40 P=99
heightOfMap :: Int
heightOfMap = 9 --B=9 I=16 P=16
widthOfMap :: Int
widthOfMap = 9 --B=9 I=16 P=30
      
--Create the grid
createBlankGrid :: Grid
createBlankGrid = (Grid  [] (genGrid 0) 0 sqsLeft 0)
    where genGrid n | n == heightOfMap = []
                    | otherwise = (Row [] (genRow n 0) 0):(genGrid (n+1))
          sqsLeft = ((heightOfMap * widthOfMap) - numberOfMines)

genRow :: Int -> Int -> [Square]
genRow row col | col == widthOfMap = []
               | otherwise = (Hidden 0 False):(genRow row (col+1))

--Perform an action on the grid
updateGrid :: Grid -> Int -> Action-> (Grid, Square)
updateGrid (Grid prevRows (currRow:nextRows) currRowIndex numLeft numFlagged) 0 action = ((Grid prevRows (newRow:nextRows) currRowIndex numLeft numFlagged), newSq)
    where (newRow, newSq) = updateRow currRow (targetColDistance currRow action) action
updateGrid (Grid prevRows (currRow:nextRows) currRowIndex numLeft numFlagged) n action | n < 0 = updateGrid (Grid (currRow:prevRows) nextRows (currRowIndex+1) numLeft numFlagged) (n+1) action
updateGrid (Grid (lastRow:prevRows) nextRows currRowIndex numLeft numFlagged) n action | n > 0 = updateGrid (Grid prevRows (lastRow:nextRows) (currRowIndex-1) numLeft numFlagged) (n-1) action

updateRow :: Row -> Int -> Action -> (Row, Square)
updateRow (Row prevSqs ((currSq):nextSqs) currColIndex) 0 (Action op coords) = ((Row prevSqs (newSq:nextSqs) currColIndex), newSq)
    where newSq = op currSq
updateRow (Row prevSqs (currSq:nextSqs) currColIndex) n action | n < 0 = updateRow (Row (currSq:prevSqs) nextSqs (currColIndex+1)) (n+1) action
updateRow (Row (lastSq:prevSqs) nextSqs currColIndex) n action | n > 0 = updateRow (Row prevSqs (lastSq:nextSqs) (currColIndex-1)) (n-1) action

--Place the mines on the field
createMinedGrid :: Grid -> StdGen -> (Int, Int) -> (Grid, StdGen)
createMinedGrid grid gen (fstRow, fstCol) = (mineTheField grid (init randomPairs), newGenerator)
    where (randomPairs, newGenerator) = genRandomCoords gen numberOfMines [(fstRow, fstCol)]

mineTheField :: Grid -> [(Int, Int)] -> Grid
mineTheField grid [] = grid
mineTheField grid (mine:mines) = mineTheField (placeMines grid actions) mines
    where actions = (Action placeMine mine):(map (Action incrAdjMineCount) (getSurrounding mine))   --place the mine and increment the surrounding tiles

placeMines :: Grid -> [Action] -> Grid
placeMines grid [] = grid
placeMines grid (action:actions) = placeMines (fst (updateGrid grid (targetRowDistance grid action) action)) actions

--Reveal a tile
clearSquare :: Grid -> (Int, Int) -> (Grid, Bool)
clearSquare grid coords = case oldSquare of (Clear _) -> (grid, True) --Tile has already been cleared
                                            _ -> case newSquare of (Mine _) -> (newGrid, False)
                                                                   _ -> (cascadeClear newGrid [coords], True)   
                                               --Need to clear first tile twice to make sure proper checks are done
    where oldSquare = internalLookupSq grid coords
          action = (Action revealTile coords)
          distance = (targetRowDistance grid action)
          (newGrid, newSquare) = updateGrid grid distance action

cascadeClear :: Grid -> [(Int, Int)] -> Grid
cascadeClear grid [] = grid
cascadeClear grid (coords:rest) = case newSquare of (Clear 0) -> isValidClear newGrid (rest ++ (getSurrounding coords))  --Infinite recursion
                                                    (Clear _) -> isValidClear newGrid rest 
                                                    _ -> error "Tile isn't revealed after it has been cleared!" --This case should never happen
    where action = (Action revealTile coords)
          (partialNewGrid, newSquare) = updateGrid grid (targetRowDistance grid action) action
          newGrid = partialNewGrid { numLeft = (numLeft partialNewGrid) - 1 }   --Find better way to do this

isValidClear :: Grid -> [(Int, Int)] -> Grid --Iterates through the list until it finds the next square which needs to be cleared
isValidClear grid [] = grid
isValidClear grid (coords:rest) = case internalLookupSq grid coords of (Hidden _ _) -> cascadeClear grid (coords:rest)
                                                                       _ -> isValidClear grid rest

--Place a flag on the grid
placeFlag :: Grid -> (Int, Int) -> Grid
placeFlag grid coords = case square of (Clear _) -> newGrid
                                       (Mine True) -> newGrid { numFlagged = (numFlagged newGrid) + 1}
                                       (Mine False) -> newGrid { numFlagged = (numFlagged newGrid) - 1}
                                       (Hidden _ True) -> newGrid { numFlagged = (numFlagged newGrid) + 1}
                                       (Hidden _ False) -> newGrid { numFlagged = (numFlagged newGrid) - 1}
    where (newGrid, square) = updateGrid grid (targetRowDistance grid action) action
          action = (Action toggleFlag coords)

--Lookup Square
lookupSquare :: Grid -> (Int, Int) -> DisplayedSquare   --Limit what the outside world can see about the square
lookupSquare grid coords = externalSq (internalLookupSq grid coords)

internalLookupSq :: Grid -> (Int, Int) -> Square
internalLookupSq grid coords = snd $ updateGrid grid distance action
    where action = (Action id coords)
          distance = (targetRowDistance grid action)

externalSq :: Square -> DisplayedSquare 
externalSq (Clear num) = Visible num
externalSq (Hidden _ flag) = Unknown flag
externalSq (Mine flag) = Unknown flag
          
--Various Actions you can perform on square
placeMine :: Square -> Square
placeMine (Hidden _ flag) = Mine flag
placeMine _ = error "Can't place a mine there"

revealTile :: Square -> Square
revealTile (Clear n) = Clear n                                
revealTile (Hidden n _) = Clear n
revealTile (Mine flag) = Mine flag

toggleFlag :: Square -> Square  --Place or remove a flag on a square
toggleFlag (Clear n) = Clear n
toggleFlag (Hidden n flag) = Hidden n (not flag)
toggleFlag (Mine flag) = Mine (not flag)

incrAdjMineCount :: Square -> Square  --Increment the number of adjacent mines
incrAdjMineCount (Clear n) = Clear (n+1)
incrAdjMineCount (Hidden n flag) = Hidden (n+1) flag
incrAdjMineCount (Mine flag) = Mine flag

--Functions for returning to the begining of a row or the grid
startOfRow :: Row -> Row
startOfRow (Row prevCols nextCols _ ) = Row [] (startOfList prevCols nextCols) 0

startOfGrid :: Grid -> Grid
startOfGrid (Grid prevRows nextRows _ numLeft numFlagged) = Grid [] (startOfList prevRows nextRows) 0 numLeft numFlagged

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

genRandomCoords :: StdGen -> Int -> [(Int, Int)] -> ([(Int, Int)], StdGen)
genRandomCoords newGen 0 list = (list, newGen)
genRandomCoords g n list = if elem (row, col) list then genRandomCoords g3 n list else genRandomCoords g3 (n-1) ((row, col):list) --Make sure the tile doesn't already have a mine in it
    where (row, g2) = randomR (0, heightOfMap-1) g
          (col, g3) = randomR (0, widthOfMap-1) g2    

getDisplayGrid :: Grid -> [[DisplayedSquare]]
getDisplayGrid grid = map getDisplayRow nextRows
    where (Grid [] nextRows _ _ _) = startOfGrid grid

getDisplayRow :: Row -> [DisplayedSquare]
getDisplayRow row = map externalSq sqs
    where (Row [] sqs _) = startOfRow row