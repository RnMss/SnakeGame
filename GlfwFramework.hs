{- 
 - FGLUT = Framework of GLUT
 - GLUT  = OpenGL utility toolkit
 
 {- Oh, nested comment is allowed! -}
  - Unlike C/C++ :
    /* /* there is */ error here!! */
 -} 

module GlfwFramework 
    ( UIEvent (..)
    , runGlfw
    , RunGlfwConf (..)
    ) where

import Graphics.Rendering.OpenGL
import Graphics.UI.GLFW
import Data.IORef
import System.Time
import Circuit
import Utils

import Control.Arrow

data UIEvent
    = UIDisplay
    | UIIdle
    | UIClose
    | UIReshape !Size
    | UIKeyDown !Key
    | UIKeyUp !Key
    | UICharDown !Char
    | UICharUp !Char
    | UIMouseDown !MouseButton
    | UIMouseUp !MouseButton
    | UIMouseMove !Position
    | UIMouseWheel !Int
    | UITimer !Double
    | UINothing    -- unused: to prevent pattern matching warning
    deriving Show

simpleInit = do
    _init_success <- initialize

    -- openWindowHint OpenGLVersionMajor 2
    -- openWindowHint OpenGLVersionMinor 1

    _win_success <- openWindow 
            ( Size 400 400 )
            [ DisplayRGBBits 8 8 8
            , DisplayAlphaBits 8 ]
            Window
    
    swapInterval $= 1
    disableSpecial AutoPollEvent

    return (_init_success && _win_success)

data RunGlfwConf = RunGlfwConf
        { confTitle :: String
        , confTimer :: Double
        }

runGlfw :: RunGlfwConf
        -> (UIEvent -> IO Bool)   -- Event Handler
        -> IO ()
runGlfw conf handler = do

    simpleInit
    windowTitle $= conf.>confTitle

    varExit  <- newIORef False

    let doEvt event = do
        _is_exit <- handler event
        varExit $= _is_exit

    windowCloseCallback   $= (doEvt UIClose >> get varExit)

    windowSizeCallback    $= onWindowSize doEvt
    windowRefreshCallback $= onWindowRefresh doEvt
    mousePosCallback      $= onMouseMove doEvt
    mouseWheelCallback    $= onMouseWheel doEvt
    charCallback          $= onChar doEvt
    mouseButtonCallback   $= onMouseButton doEvt
    keyCallback           $= onKey doEvt

    firstTime <- get time 

    let timerIntv = conf.>confTimer
    let loopWithTimer nextTime = do
        pollEvents
        q <- get varExit
        if q then 
            return ()
        else do
            curTime <- get time
            if curTime < nextTime
            then do 
                sleep 0.002
                loopWithTimer nextTime
            else do
                doEvt (UITimer curTime)
                loopWithTimer (curTime + timerIntv)
    
    firstTime <- get time 
    loopWithTimer (firstTime+0.05)

onMouseMove doEvt pos =
    doEvt (UIMouseMove pos)

onMouseWheel doEvt amnt =
    doEvt (UIMouseWheel amnt)

onWindowSize doEvt size =
    doEvt (UIReshape size)

onWindowRefresh doEvt =
    doEvt UIDisplay

onChar doEvt char state =
    let event = case state of { 
        Press   -> UICharDown char;
        Release -> UICharUp char;
    } in doEvt event

onMouseButton doEvt button state =
    let event = case state of { 
        Press   -> UIMouseDown button;
        Release -> UIMouseUp button;
    } in doEvt event

onKey doEvt key state =
    let event = case state of {
        Press   -> UIKeyDown key;
        Release -> UIKeyUp key;
    } in doEvt event