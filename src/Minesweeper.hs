module Minesweeper  (
    Grid (..),
    DisplayedSquare (..),
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
import Lib

--Zipper style lists, allows us to easily go backwards and forwards through the list
data Grid = Grid { prevRows :: [Row], nextRows :: [Row], currRowIndex :: Int, numLeft :: Int, numFlagged :: Int } 

data Row = Row { prevCols :: [Square], nextCols :: [Square], currColIndex :: Int }

data DisplayedSquare = Visible Int  --Form that can be viewed by external elements
                    | Unknown Bool

data Square = Clear Int
            | Hidden Int Bool
            | Mine Bool

data Action = Action (Square->Square) (Int, Int)    --Takes the coordinates of a square and a function to run on that square

--This stuff is used for terminal version of the game
newtype NoQuotes = NoQuotes String      --Taken from https://www.reddit.com/r/haskell/comments/blxveo/print_out_string_without_double_quotes/
instance Show NoQuotes where show (NoQuotes str) = str

instance Show Grid where
    show ( Grid [] [] _ _ _ ) = ""
    show ( Grid [] (row:rows) currRow numLeft numFlagged ) = (show row) ++ "\n" ++ ( show (Grid [] rows currRow numLeft numFlagged) )
    show grid = show $ startOfGrid grid --Make sure we display the rows in the correct order

instance Show Row where
    show ( Row [] nextSqs _ ) = (show nextSqs)
    show row = show $ startOfRow row --Make sure we display the columns in the correct order

instance Show Square where
    show (Clear n) = show n --Revealed square
    show (Hidden _ True) = show (NoQuotes "F") --Flagged square
    show (Mine True) = show (NoQuotes "F") --Flagged square
    show _ = show (NoQuotes " ")    --Unknown may be a mine or empty

--Declare Constants
numberOfMines :: Int
numberOfMines = 40 --B=10 I=40 P=99
heightOfMap :: Int
heightOfMap = 16 --B=9 I=16 P=16
widthOfMap :: Int
widthOfMap = 16 --B=9 I=16 P=30
      
--Create the grid
createBlankGrid :: Grid
createBlankGrid = (Grid  [] rows 0 sqsLeft 0)
          where sqsLeft = ((heightOfMap * widthOfMap) - numberOfMines)
                row = (Row [] (take widthOfMap $ repeat (Hidden 0 False)) 0)     --All squares and rows are initialized to default values
                rows = take heightOfMap $ repeat row

--Perform an action on the grid
updateGrid :: Grid -> Action-> (Grid, Square)
updateGrid (Grid prevRows ftrRows currRowInd numLeft numFlagged) (Action move (targRow, targCol)) =
    ((Grid newPrevRows (newCurrRow:newftrRows) targRow numLeft numFlagged), newSq)  --Update grid
        where (newPrevRows, ((Row prevSqs ftrSqs currSqInd):newftrRows)) = moveZipList prevRows ftrRows (currRowInd - targRow)  --Get row
              (newPrevSqs, (square:newftrSqs)) = moveZipList prevSqs ftrSqs (currSqInd - targCol)   --Get square
              newSq = move square   --Update square
              newCurrRow = (Row newPrevSqs (newSq:newftrSqs) targCol) --Update row

moveZipList :: [a] -> [a] -> Int -> ([a], [a]) 
moveZipList prevs ftrs dist | dist == 0 = (prevs, ftrs)
moveZipList prevs (curr:ftrs) dist | dist < 0 = moveZipList (curr:prevs) ftrs (dist+1)
moveZipList (last:prevs) ftrs dist | dist > 0 = moveZipList prevs (last:ftrs) (dist-1)

--Place the mines on the field
createMinedGrid :: Grid -> StdGen -> (Int, Int) -> (Grid, StdGen)
createMinedGrid grid gen fstGuess = (mineTheField grid (init randomPairs), newGenerator) --Use init so we place all the mines except leave out the first guess
    where (randomPairs, newGenerator) = genRandomCoords gen numberOfMines [fstGuess]    --We pass the first guess in so a mine can't be place in that tile

mineTheField :: Grid -> [(Int, Int)] -> Grid
mineTheField grid [] = grid
mineTheField grid (mine:mines) = mineTheField (placeMines grid actions) mines
    where actions = (Action placeMine mine):(map (Action incrAdjMineCount) (getSurrounding mine))   --place the mine and increment the surrounding tiles

placeMines :: Grid -> [Action] -> Grid
placeMines grid [] = grid
placeMines grid (action:actions) = placeMines (fst (updateGrid grid action)) actions    --TODO

--Reveal a tile
clearSquare :: Grid -> (Int, Int) -> (Grid, Bool)   --TODO return something better than bool so we can explode the mine
clearSquare grid coords = case square of (Clear _) -> (grid, True) --Tile has already been cleared
                                         (Mine _) -> (grid, False)
                                         (Hidden _ _) -> (cascadeClear grid [coords], True) --Clear tile and cascade  
    where square = internalLookupSq grid coords

cascadeClear :: Grid -> [(Int, Int)] -> Grid
cascadeClear grid [] = grid
cascadeClear grid (coords:rest) = 
    case oldSq of (Hidden _ _) -> case newSquare of (Clear 0) -> cascadeClear newGrid ((getSurrounding coords) ++ rest) 
                                                    (Clear _) -> cascadeClear newGrid rest 
                                                    _ -> error "Tile isn't revealed after it has been cleared!" --This case should never happen
                  otherwise -> cascadeClear grid rest
        where oldSq = internalLookupSq grid coords
              (partialNewGrid, newSquare) = updateGrid grid (Action revealTile coords)
              newGrid = partialNewGrid { numLeft = (numLeft partialNewGrid) - 1 } 

--Place a flag on the grid
placeFlag :: Grid -> (Int, Int) -> Grid
placeFlag grid coords = case square of (Clear _) -> newGrid
                                       (Mine True) -> newGrid { numFlagged = (numFlagged newGrid) + 1}  --Update flag number if appropriate
                                       (Mine False) -> newGrid { numFlagged = (numFlagged newGrid) - 1}
                                       (Hidden _ True) -> newGrid { numFlagged = (numFlagged newGrid) + 1}
                                       (Hidden _ False) -> newGrid { numFlagged = (numFlagged newGrid) - 1}
    where (newGrid, square) = updateGrid grid (Action toggleFlag coords)

--Lookup Square
lookupSquare :: Grid -> (Int, Int) -> DisplayedSquare   --Limit what the outside world can see about the square
lookupSquare grid coords = externalSq (internalLookupSq grid coords)

internalLookupSq :: Grid -> (Int, Int) -> Square
internalLookupSq grid coords = snd $ updateGrid grid (Action id coords)

externalSq :: Square -> DisplayedSquare --Convert a square to form that can be viewed by external functions
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
startOfRow (Row prevCols nextCols _ ) = Row [] (combine prevCols nextCols) 0

startOfGrid :: Grid -> Grid
startOfGrid (Grid prevRows nextRows _ numLeft numFlagged) = Grid [] (combine prevRows nextRows) 0 numLeft numFlagged

combine :: [a] -> [a] -> [a]    --Combines two lists
combine [] ys = ys
combine (x:xs) ys = combine xs (x:ys)

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