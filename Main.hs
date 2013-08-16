import Graphics.Rendering.OpenGL
import Graphics.UI.GLFW
import Control.Monad
import Data.Char
import Data.IORef

import GlfwFramework
import Circuit
import SnakeGame
import qualified SnakeGame as S
import SnakeRender

import Randomize
import Utils

main = do
    game <- runRandIO $ randSnakeGame (15, 15)
    vGame <- newIORef game
    vOper <- newIORef S.U

    runGlfw (RunGlfwConf "Hello" 0.2)
            (handleEvents (vOper, vGame))

handleEvents :: (IORef Operation, IORef SnakeGame)
             -> UIEvent -> IO Bool
handleEvents (vOper, vGame) event = do

    dealEvent event
    where  
    dealEvent :: UIEvent -> IO Bool 
    dealEvent UIDisplay = do

        clear [ColorBuffer]

        game <- readIORef vGame
        renderGame game

        let title = if dead game 
            then "Dead" else "Snake"
        windowTitle $= title
            
        flush
        swapBuffers

        return False

    dealEvent (UITimer clk) = do

        game <- get vGame
        oper <- get vOper

        nextGame <- runRandIO (nextSnakeState game oper)
        vGame $=! nextGame

        dealEvent UIDisplay

    dealEvent (UIReshape size) = do
        
        viewport $= (Position 0 0, size)
        dealEvent UIDisplay

    dealEvent (UIKeyDown key) =
        let 
            f (CharKey 'W') =
                modifyIORef' vOper (nextSnakeOpr S.U)
            f (CharKey 'A') =           
                modifyIORef' vOper (nextSnakeOpr S.L)
            f (CharKey 'S') =
                modifyIORef' vOper (nextSnakeOpr S.D)
            f (CharKey 'D') =
                modifyIORef' vOper (nextSnakeOpr S.R)

            f (CharKey 'O') = do
                game <- runRandIO (randSnakeGame (15, 15))
                vGame $= game

            f _ = return ()

        in f key >> return False

    dealEvent UIClose = return True
    dealEvent _ = return False

nextSnakeOpr :: Operation -> Operation -> Operation
nextSnakeOpr newOpr oldOpr =
    if opposite oldOpr newOpr then oldOpr else newOpr
    
opposite :: Operation -> Operation -> Bool
opposite S.U S.D = True
opposite S.D S.U = True
opposite S.L S.R = True
opposite S.R S.L = True
opposite _ _ = False