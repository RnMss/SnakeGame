import Graphics.Rendering.OpenGL
import Graphics.UI.GLFW
import Control.Monad
import Data.Char
import Data.IORef

-- import FGLUT
import GameStates
import SnakeGame
import qualified SnakeGame( SnakeOperation (U, D, L, R) )
import SnakeRender

import Randomize
import Utils

main = do 
    initSuccess <- initialize

    winSuccess <-
        openWindow  ( Size 400 400 )
                    [ DisplayRGBBits 8 8 8
                    , DisplayAlphaBits 8 ]
                    Window

    windowTitle $= "Hello"
    clearColor $= Color4 0 0 0 0
    swapInterval $= (1000 `div` 60)

    game <- runRandIO ( randSnakeGame (15, 15) )

    windowClosed <- newIORef False

    disableSpecial AutoPollEvent

    windowSizeCallback $= \(Size w h) ->
        do  viewport $= (Position 0 0, Size w h)
            redraw game

    windowRefreshCallback $= 
        do  redraw game

    windowCloseCallback $=
        do  windowClosed $= True
            return True

    firstTime <- get time 
    let loopTimer nextTime = do
        pollEvents
        q <- get windowClosed
        when (not q) $ do
            curTime <- get time
            if curTime < nextTime
            then do 
                sleep 0.002
                loopTimer nextTime
            else do
                redraw game
                loopTimer (curTime+0.015)

    loopTimer (firstTime+0.05)

redraw game =
    do  clear [ColorBuffer]
        renderGame game
        flush
        swapBuffers

{-

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

-}