import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT

import Data.Char

import FGLUT
import GameStates
import SnakeGame
import qualified SnakeGame( SnakeOperation (U, D, L, R) )
import SnakeRender

import Randomize
import Utils

main = do 
    (progName, _) <- getArgsAndInitialize
    newGS <- runRandIO newGSRand
    startGlut ("Snake") (200) (newGS) (dealEvent)

dealEvent :: GS -> Event -> IO GS 
dealEvent state EventDisplay = do
    clear [ColorBuffer]

    renderGame (state.>world)

    windowTitle $= if (state.>world.>dead) 
                    then "Dead"
                    else "Snake"

    flush

    return state

dealEvent state (EventTimer clk) = do
    nextWorld <- runRandIO (nextSnakeState sw operation)
    return state { 
        world = nextWorld
    }

    where
    si = state.>input
    sw = state.>world
    operation = si.>lastOperation


dealEvent state (EventReshape (Size w h)) = do
    viewport $= (Position 0 0, Size w h)
    postRedisplay Nothing
    return state

dealEvent state (EventKeyDown key position) =
    if key' == 'o' 
        then runRandIO newGSRand
        else return state { input = next (state.>input) key' }

    where
    key' = toLower key

    next :: GSInput -> Char -> GSInput
    next si 'w' = si { lastOperation = SnakeGame.U }
    next si 'a' = si { lastOperation = SnakeGame.L }
    next si 's' = si { lastOperation = SnakeGame.D }
    next si 'd' = si { lastOperation = SnakeGame.R }
    next si _   = si

dealEvent state _ = return state