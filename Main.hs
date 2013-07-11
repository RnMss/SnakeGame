import Graphics.Rendering.OpenGL
import Graphics.UI.GLFW
import Control.Monad
import Data.Char
import Data.IORef

import GlfwFramework
import Circuit
import GameStates
import SnakeGame
import qualified SnakeGame( SnakeOperation (..) )
import SnakeRender

import Randomize
import Utils

main = do
    sf <- runRand handlerRand
    runGlfw "Hello" (1000 / 60) sf

handlerRand :: Rand (Circuit UIEvent (IO Bool))
handlerRand = do
    gameInit <- randSnakeGame 
    return handlerC

handlerC gameInit =
    proc event -> do
    e_res <- reshaped -< event
    e_ref <- refreshed -< event
    let redrawAction =
        if isJust e_res || 
           isJust e_ref 
        then Just (redraw game)
        else Nothing

    operation <- snakeOperation -< event
    rec game <- regC gameInit -< gameNext
        let gameNext = fmap (nextSnakeState game) operation

    returnA $ or_M $ catMaybes $
        [ redrawAction ]



or_M, and_M :: Monad m => [m Bool] -> m Bool
or_M  xs = sequence xs >>= (return . or )
and_M xs = sequence xs >>= (return . and)


redraw game =
    do  clear [ColorBuffer]
        renderGame game
        flush
        swapBuffers
        return False

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