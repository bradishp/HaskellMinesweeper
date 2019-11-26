module Main where

import System.Random
import System.IO
import Minesweeper
import AutoSolver
import Lib

--ThreePenny stuff
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import Data.IORef
import Control.Monad.Trans (liftIO)

data Modes = Uncover | Flag

data GameState = Ongoing Bool Int Int | Victory Int Int | GameOver Int Int  --Bool says whether it is first move or not

sqSize :: Num a => a    --Adjust this to adjust the size of the grid
sqSize = 50

--Ideas
-- Pass stdGen in some state so program isn't as messy

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
    gameState <- liftIO $ newIORef (Ongoing True 0 0)

    --Buttons
    uncoverMode <- UI.button #+ [string "Uncover"]
    flagMode <- UI.button #+ [string "Flag"]
    aiMove <- UI.button #+ [string "Make AI move"]
    aiSolve <- UI.button #+ [string "AI solve grid"]
    reset <- UI.button #+ [string "New game"]
    gameLog <- UI.h1 #+ [string "Game Log"]
    getBody window #+ [column [element canvas], 
                        element uncoverMode, element flagMode, element aiMove, element aiSolve, 
                        element reset, element gameLog]

    --Setup minesweeper grid and populate it
    generator <- liftIO $ newStdGen
    let initialGrid = createBlankGrid
    gen <- liftIO $ newIORef generator
    grid <- liftIO $ newIORef initialGrid
    displayGrid canvas (getDisplayGrid initialGrid) 0 0  
  
    --Switch between flag and uncover modes
    on UI.click uncoverMode $ \_ ->
        do liftIO $ writeIORef mode Uncover
    on UI.click flagMode $ \_ ->
        do liftIO $ writeIORef mode Flag

    --Single AI move
    on UI.click aiMove $ \_ -> do
        makeAiMove window canvas grid gen gameState computerMove

    --Get AI to solve the whole grid
    on UI.click aiSolve $ \_ -> do
        gs <- liftIO $ readIORef gameState
        case gs of 
            (Ongoing True _ _) -> do   --First move so run function twice once to do random first move and once to do rest
                makeAiMove window canvas grid gen gameState computerSolve
                makeAiMove window canvas grid gen gameState computerSolve
            (Ongoing False _ _) -> do
                makeAiMove window canvas grid gen gameState computerSolve
            otherwise -> do
                return()
        
    --New game
    on UI.click reset $ \_ -> do
       do newGame canvas grid gen gameState

    --Keep track of the position of the mouse
    on UI.mousemove canvas $ \xy -> do 
        liftIO $ writeIORef pos xy

    --Handle user clicking on the tile
    on UI.click canvas $ \_  -> do
        gs <- liftIO $ readIORef gameState
        case gs of
            (Ongoing firstMove _ _) -> do
                (x, y) <- liftIO $ readIORef pos
                let (col, row) = coordsToIndex (x, y)
                currGrid <- liftIO $ readIORef grid
                m <- liftIO $ readIORef mode
                case m of
                    Uncover -> do
                        case firstMove of
                            True -> do
                                firstTurn grid gen gameState (row, col)
                                makeClearMove window canvas grid gameState (row, col) 
                            False -> do
                                makeClearMove window canvas grid gameState (row, col) 
                    Flag -> do
                        let newGrid = placeFlag currGrid (row, col)
                        liftIO $ writeIORef grid newGrid
                        liftIO $ print newGrid
                        displayGrid canvas (getDisplayGrid newGrid) 0 0
                        checkGameState window newGrid True gameState
            _ -> return ()

--Handle types of moves
makeClearMove :: Window -> UI.Canvas -> IORef Grid -> IORef GameState -> (Int, Int) -> UI ()
makeClearMove window canvas grid gameState (row, col) = do
    currGrid <- liftIO $ readIORef grid
    let (newGrid, success) = clearSquare currGrid (row, col)
    liftIO $ writeIORef grid newGrid
    liftIO $ print newGrid
    displayGrid canvas (getDisplayGrid newGrid) 0 0
    checkGameState window newGrid success gameState 

--Handle both single and multi AI moves
makeAiMove :: Window -> UI.Canvas -> IORef Grid -> IORef StdGen -> IORef GameState -> (Grid -> StdGen -> (Grid, Bool)) -> UI ()
makeAiMove window canvas grid gen gameState playMoveType = do
    gs <- liftIO $ readIORef gameState
    case gs of
        (Ongoing firstMove _ _) -> do
            case firstMove of
                True -> do      --First move so just make a random guess
                    generator <- liftIO $ readIORef gen
                    let ([fstGuess], newGen) = genRandomCoords generator 1 []
                    liftIO $ writeIORef gen newGen
                    firstTurn grid gen gameState fstGuess
                    makeClearMove window canvas grid gameState fstGuess --Make move
                False -> do
                    currGrid <- liftIO $ readIORef grid
                    generator <- liftIO $ readIORef gen --Don't need to bother updating generator as it will just guess the same values until it hits a new one
                    let (newGrid, success) = playMoveType currGrid generator
                    liftIO $ writeIORef grid newGrid
                    liftIO $ print newGrid
                    displayGrid canvas (getDisplayGrid newGrid) 0 0
                    checkGameState window newGrid success gameState 
        _ ->
            return ()

firstTurn :: IORef Grid -> IORef StdGen -> IORef GameState -> (Int, Int) -> UI() 
firstTurn grid gen gameState fstGuess = do
    currGrid <- liftIO $ readIORef grid
    currGen <- liftIO $ readIORef gen
    let (newGrid, newGen) = createMinedGrid currGrid currGen fstGuess
    liftIO $ writeIORef grid newGrid
    liftIO $ writeIORef gen newGen
    currGameState <- liftIO $ readIORef gameState
    let (wins, played) = getWinRatio currGameState
    liftIO $ writeIORef gameState (Ongoing False wins played)

--Checks if the game is over and updates the game accordingly
checkGameState :: Window -> Grid -> Bool -> IORef GameState -> UI ()
checkGameState window _ False gameState = do
    currGameState <- liftIO $ readIORef gameState
    let (wins, played) = getWinRatio currGameState
    getBody window #+ [ UI.p #+ [string "You Lost a Game!"] ]
    getBody window #+ [ UI.p #+ [string ("Current record: " ++ (show wins) ++ " wins in " ++ (show (played+1)) ++ " games")] ]
    liftIO $ writeIORef gameState (GameOver wins (played+1))
    return ()
checkGameState window (Grid _ _ _ 0 _) True gameState = do
    currGameState <- liftIO $ readIORef gameState
    let (wins, played) = getWinRatio currGameState
    getBody window #+ [ UI.p #+ [ string "You Won a Game!"] ]
    getBody window #+ [ UI.p #+ [string ("Current record: " ++ (show (wins+1)) ++ " wins in " ++ (show (played+1)) ++ " games")] ]
    liftIO $ writeIORef gameState (Victory (wins+1) (played+1))
    return ()
checkGameState _ _ _ _ = return ()  --Game is still ongoing

{-defeat :: Window -> Canvas -> (Int, Int) IORefGameState -> UI ()
defeat window canvas coords gameState = do
    currGameState <- liftIO $ readIORef gameState
    let (wins, played) = getWinRatio currGameState
    getBody window #+ [ UI.p #+ [string "You Lost a Game!"] ]
    getBody window #+ [ UI.p #+ [string ("Current record: " ++ (show wins) ++ " wins in " ++ (show (played+1)) ++ " games")] ]
    liftIO $ writeIORef gameState (GameOver wins (played+1))
    return ()

victory :: Window -> IORefGameState -> UI ()
victory window gameState = do
    currGameState <- liftIO $ readIORef gameState
    let (wins, played) = getWinRatio currGameState
    getBody window #+ [ UI.p #+ [ string "You Won a Game!"] ]
    getBody window #+ [ UI.p #+ [string ("Current record: " ++ (show (wins+1)) ++ " wins in " ++ (show (played+1)) ++ " games")] ]
    liftIO $ writeIORef gameState (Victory (wins+1) (played+1))
    return ()-}

newGame :: UI.Canvas -> IORef Grid -> IORef StdGen-> IORef GameState -> UI()
newGame canvas grid gen gameState = do 
    generator <- liftIO $ newStdGen --Maybe shouldn't get a new one
    let initialGrid = createBlankGrid
    liftIO $ writeIORef gen generator
    liftIO $ writeIORef grid initialGrid
    currGameState <- liftIO $ readIORef gameState
    let (wins, played) = getWinRatio currGameState
    liftIO $ writeIORef gameState (Ongoing True wins played)
    displayGrid canvas (getDisplayGrid initialGrid) 0 0      --Display grid on the screen

--Display mine grid
displayGrid :: UI.Canvas -> [[DisplayedSquare]] -> Int -> Int -> UI ()
displayGrid _ [] _ _ = return ()
displayGrid canvas ([]:rest) rowNum colNum = displayGrid canvas rest (rowNum+1) 0
displayGrid canvas ((sqr:sqrs):rest) rowNum colNum = do
    drawSq canvas sqr (colNum*sqSize, rowNum*sqSize)
    displayGrid canvas (sqrs:rest) rowNum (colNum+1)

drawSq :: UI.Canvas -> DisplayedSquare -> (Int, Int) -> UI ()
drawSq canvas (Visible 0) (xPos, yPos) = do
    canvas # set' UI.fillStyle (UI.htmlColor "white")
    canvas # UI.fillRect (fromIntegral xPos,fromIntegral yPos) sqSize sqSize
drawSq canvas (Visible numAdj) (xPos, yPos) = do
    canvas # set' UI.fillStyle (UI.htmlColor "white")
    canvas # UI.fillRect (fromIntegral xPos,fromIntegral yPos) sqSize sqSize
    --Now display number of adjacent mines
    canvas # set' UI.fillStyle (UI.htmlColor "red")
    canvas # set' UI.textAlign UI.Center
    canvas # set' UI.textFont "20px sans-serif"
    canvas # UI.fillText (show numAdj) (fromIntegral (xPos+25),fromIntegral (yPos+25))
drawSq canvas (Unknown False) (xPos, yPos) = do
    --canvas # set' UI.strokeStyle ("red")    --Not working
    canvas # set' UI.fillStyle (colourSq (xPos, yPos))
    canvas # UI.fillRect (fromIntegral xPos,fromIntegral yPos) sqSize sqSize
    --canvas # UI.stroke
drawSq canvas (Unknown True) (xPos, yPos) = do  --Flagged
    canvas # set' UI.fillStyle (colourSq (xPos, yPos))
    canvas # UI.fillRect (fromIntegral xPos,fromIntegral yPos) sqSize sqSize
    canvas # set' UI.fillStyle (UI.htmlColor "red")
    canvas # set' UI.textAlign UI.Center
    canvas # set' UI.textFont "20px sans-serif"
    canvas # UI.fillText "F" (fromIntegral (xPos+25),fromIntegral (yPos+25))

--Helper functions
coordsToIndex :: (Int, Int) -> (Int, Int)
coordsToIndex (x, y) = (x `div` sqSize, y `div` sqSize)

colourSq :: (Int, Int) -> UI.FillStyle        --Alternates the colour of the grid squares
colourSq (xPos, yPos) | even ((xPos `div` sqSize) + (yPos `div` sqSize)) = UI.solidColor (UI.RGB 90 90 90)
                      | otherwise = UI.solidColor (UI.RGB 180 180 180)

getWinRatio :: GameState -> (Int, Int)
getWinRatio (Ongoing _ wins played) = (wins, played)
getWinRatio (GameOver wins played) = (wins, played)
getWinRatio (Victory wins played) = (wins, played)