module Main where

import Lib
import System.Random
import System.IO
import AutoSolver
import Minesweeper

--ThreePenny stuff
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import Data.IORef
import Control.Monad.Trans (liftIO)

data Modes = Uncover | Flag

data GameState = Ongoing | Victory | GameOver

sqSize :: Num a => a
sqSize = 50

--Ideas
-- Pass stdGen in some state so program isn't as messy
-- Remove coords from square as they aren't really needed and use up a lot of space

--Setup the game
main :: IO ()
main = do
    startGUI defaultConfig setup

setup window = do
    --Create the visual grid
    return window # set title "Minesweeper"
    canvas <- UI.canvas
      # set UI.height (heightOfMap * sqSize)
      # set UI.width (widthOfMap * sqSize)
      # set UI.style [("border", "solid black 1px"), ("background", "#eee")]

    --Variables
    pos <- liftIO $ newIORef (0,0)
    mode <- liftIO $ newIORef Uncover
    gameState <- liftIO $ newIORef Ongoing

    --Buttons and text boxes
    uncoverMode <- UI.button #+ [string "Uncover"]
    flagMode <- UI.button #+ [string "Flag"]
    aiMove <- UI.button #+ [string "Make AI move"]
    getBody window #+ [column [element canvas], element uncoverMode, element flagMode, element aiMove]

    --Setup minesweeper grid and populate it
    generator <- liftIO $ newStdGen
    let (initialGrid, newGenerator) = setupGrid generator
    gen <- liftIO $ newIORef newGenerator
    grid <- liftIO $ newIORef initialGrid
    displayCanvas canvas initialGrid      --Display grid on the screen
  
    --Switch between flag and uncover modes
    on UI.click uncoverMode $ \_ ->
        do liftIO $ writeIORef mode Uncover
    on UI.click flagMode $ \_ ->
        do liftIO $ writeIORef mode Flag

    on UI.click aiMove $ \_ -> do
        gs <- liftIO $ readIORef gameState
        case gs of
            Ongoing -> do
                    currGrid <- liftIO $ readIORef grid
                    generator <- liftIO $ readIORef gen --Don't need to bother updating generator as it will just guess the same values until it hits a new one
                    let (newGrid, success) = computerMove currGrid generator
                    liftIO $ writeIORef grid newGrid
                    liftIO $ print newGrid
                    displayCanvas canvas newGrid
                    checkGameState window newGrid success gameState
            _ ->
                return ()

    --Keep track of the position of the mouse
    on UI.mousemove canvas $ \xy -> do 
        liftIO $ writeIORef pos xy

    --Handle user clicking on the tile
    on UI.click canvas $ \_  -> do
        gs <- liftIO $ readIORef gameState
        case gs of
            Ongoing -> do
                (x, y) <- liftIO $ readIORef pos
                let (col, row) = coordsToIndex (x, y)
                currGrid <- liftIO $ readIORef grid
                m <- liftIO $ readIORef mode
                case m of
                    Uncover -> do
                        let (newGrid, hitMine) = clearSquare currGrid (row, col)
                        liftIO $ writeIORef grid newGrid
                        liftIO $ print newGrid
                        displayCanvas canvas newGrid 
                        checkGameState window newGrid hitMine gameState  
                    Flag -> do
                        let newGrid = placeFlag currGrid (row, col)
                        liftIO $ writeIORef grid newGrid
                        liftIO $ print newGrid
                        displayCanvas canvas newGrid
                        checkGameState window newGrid True gameState
            _ -> return ()

displayCanvas :: UI.Canvas -> Grid -> UI ()
displayCanvas canvas ( Grid [] [] _ _ _ ) = return ()
displayCanvas canvas ( Grid [] (row:rows) rowNum numLeft numFlagged ) = 
    do displayRow canvas (startOfRow row) rowNum
       displayCanvas canvas (Grid [] rows (rowNum+1) numLeft numFlagged)
displayCanvas canvas grid = displayCanvas canvas (startOfGrid grid)     --Ensure that we start displaying the grid from the start

displayRow :: UI.Canvas -> Row -> Int -> UI()
displayRow canvas (Row [] [] _) _ = return ()
displayRow canvas (Row [] (sqr:sqrs) colNum) rowNum = do
                                        drawSq canvas sqr (colNum*sqSize, rowNum*sqSize)
                                        displayRow canvas (Row [] sqrs (colNum+1)) rowNum

drawSq :: UI.Canvas -> Square -> (Int, Int) -> UI ()
drawSq canvas (Clear 0 (x, y)) (xPos, yPos) = do
    canvas # set' UI.fillStyle (UI.htmlColor "white")
    canvas # UI.fillRect (fromIntegral xPos,fromIntegral yPos) sqSize sqSize
drawSq canvas (Clear numAdj (x, y)) (xPos, yPos) = do
    canvas # set' UI.fillStyle (UI.htmlColor "white")
    canvas # UI.fillRect (fromIntegral xPos,fromIntegral yPos) sqSize sqSize
    --Now display number of adjacent mines
    canvas # set' UI.fillStyle (UI.htmlColor "red")
    canvas # set' UI.textAlign UI.Center
    canvas # set' UI.textFont "20px sans-serif"
    canvas # UI.fillText (show numAdj) (fromIntegral (xPos+25),fromIntegral (yPos+25))
drawSq canvas (Hidden _ False (x, y)) (xPos, yPos) = do
    --canvas # set' UI.strokeStyle ("red")    --Not working
    canvas # set' UI.fillStyle (greyOrGray (xPos, yPos))
    canvas # UI.fillRect (fromIntegral xPos,fromIntegral yPos) sqSize sqSize
    --canvas # UI.stroke
drawSq canvas (Hidden _ True (x, y)) (xPos, yPos) = do
    canvas # set' UI.fillStyle (greyOrGray (xPos, yPos))
    canvas # UI.fillRect (fromIntegral xPos,fromIntegral yPos) sqSize sqSize
    canvas # set' UI.fillStyle (UI.htmlColor "red")
    canvas # set' UI.textAlign UI.Center
    canvas # set' UI.textFont "20px sans-serif"
    canvas # UI.fillText "F" (fromIntegral (xPos+25),fromIntegral (yPos+25))
drawSq canvas (Mine False (x, y)) (xPos, yPos) = do
    canvas # set' UI.fillStyle (greyOrGray (xPos, yPos))
    canvas # UI.fillRect (fromIntegral xPos,fromIntegral yPos) sqSize sqSize
drawSq canvas (Mine True (x, y)) (xPos, yPos) = do
    canvas # set' UI.fillStyle (greyOrGray (xPos, yPos))
    canvas # UI.fillRect (fromIntegral xPos,fromIntegral yPos) sqSize sqSize
    canvas # set' UI.fillStyle (UI.htmlColor "red")
    canvas # set' UI.textAlign UI.Center
    canvas # set' UI.textFont "20px sans-serif"
    canvas # UI.fillText "F" (fromIntegral (xPos+25),fromIntegral (yPos+25))


--Helper functions
coordsToIndex :: (Int, Int) -> (Int, Int)
coordsToIndex (x, y) = (x `div` sqSize, y `div` sqSize)

greyOrGray :: (Int, Int) -> UI.FillStyle        --Alternates the colour of the grid squares
greyOrGray (xPos, yPos) = if even ((xPos `div` sqSize) + (yPos `div` sqSize)) then (UI.solidColor (UI.RGB 90 90 90)) else (UI.solidColor (UI.RGB 180 180 180))

checkGameState :: Window -> Grid -> Bool -> IORef GameState -> UI ()
checkGameState window _ False gameState = do
    getBody window #+ [ UI.h1 #+ [ string "You Lose!"] ]
    liftIO $ writeIORef gameState GameOver
    return ()
checkGameState window (Grid _ _ _ 0 _) True gameState = do
        getBody window #+ [ UI.h1 #+ [ string "You Win!"] ]
        liftIO $ writeIORef gameState Victory
        return ()
checkGameState _ _ _ _ = return ()  --Game is still ongoing