{- 
 - FGLUT = Framework of GLUT
 - GLUT  = OpenGL utility toolkit
 
 {- Oh, nested comment is allowed! -}
 Unlike C/C++ :
 /* /* there is */ error here!! */
 -} 

module GlfwFramework 
    ( UIEvent (..)
    , runGlfw
    ) where

import Graphics.Rendering.OpenGL
import Graphics.UI.GLFW
import Data.IORef
import System.Time
import Circuit

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
    deriving Eq

runGlfw :: String                     -- Window Title
        -> Double                     -- Timer Interval
        -> Circuit UIEvent (IO Bool)  -- Event Handler (Arrow, Yampa-like somthing)
        -> IO ()
runGlfw title timerIntv circuit = do
    _init_success <- initialize

    _win_success <-
        openWindow  ( Size 400 400 )
                    [ DisplayRGBBits 8 8 8
                    , DisplayAlphaBits 8 ]
                    Window

    _main_win <- openWindow dispOptions
    
    windowTitle $= title
    swapInterval $= round timerIntv

    _will_exit   <- newIORef False
    _signal_func <- newIORef circuit 

    disableSpecial AutoPollEvent

    let handleEvent event =
        do  _sf <- get _signal_func
            let (_out, _sf_next) = runCircuit _sf event
            _signal_func $= _sf_next
            _exit <- _out
            _will_exit $~! ( _exit || ) 

    windowSizeCallback    $= \s -> handleEvent (UIReshape s)
    windowRefreshCallback $=       handleEvent (UIDisplay)
    mousePosCallback      $= \p -> handleEvent (UIMouseMove p)
    mouseWheelCallback    $= \a -> handleEvent (UIMouseWheel a)
    
    charCallback $= \char state ->
        let event = case state of 
            Press   -> UICharDown button
            Release -> UICharUp button
        in handleEvent event

    mouseButtonCallback $= \button state ->
        let event = case state of 
            Press   -> UIMouseDown button
            Release -> UIMouseUp button
        in handleEvent event

    keyCallback $= \key state -> 
        let event = case state of
            Press   -> UIKeyDown key
            Release -> UIKeyUp key
        in handleEvent event

    windowCloseCallback $= (handleEvent UIClose >> get _will_exit)

    firstTime <- get time 
    let loopTimer nextTime = do
        pollEvents
        q <- get _will_exit
        when (not q) $ do
            curTime <- get time
            if curTime < nextTime
            then do 
                sleep 0.002
                loopTimer nextTime
            else do
                handleEvent (UITimer curTime)
                loopTimer (curTime + timerIntv)
    
    firstTime <- get time 
    loopTimer (firstTime+0.05)

reshaped :: Circuit UIEvent (Maybe Size)
reshaped = arr $ \event -> case 
                UIReshape size -> Just size
                _              -> Nothing

refreshed :: Circuit UIEvent (Maybe ())
refreshed = arr $ \event -> case
                UIDisplay -> Just ()
                _         -> Nothing


