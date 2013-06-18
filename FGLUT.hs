{- 
 - FGLUT = Framework of GLUT
 - GLUT  = OpenGL utility toolkit
 
 {- Oh, nested comment is allowed! -}
 Unlike C/C++ :
 /* /* there is */ error here!! */
 -} 

module FGLUT 
    ( Event (..)
    , startGlut
    ) where

import Graphics.Rendering.OpenGL
import Graphics.UI.GLFW
import Data.IORef
import System.Time
-- import System.Clock  : Can't compile this in MacOS!!

data Event
    = EventDisplay
    | EventIdle
    | EventReshape Size
    | EventKeyDown Char Position
    | EventKeyUp Char Position
    | EventMouseDown MouseButton Position
    | EventMouseUp MouseButton Position
    | EventMouseMove Position
    | EventTimer Integer
    | EventVoid

startGlut :: String                         -- Window Title
          -> Int                            -- Timer Interval
          -> state                          -- Init State
          -> (state -> Event -> IO state)   -- Event Handler
          -> IO ()
startGlut title timerIntv initState onEvent = do
    initSuccess <- initialize

    let dispOptions = defaultDisplatOptions
                    { displayOptions_width  = 400
                    , displayOptions_height = 400
                    }

    mainWin <- openWindow dispOptions

    world <- newIORef initState
    
    let evt = eventHandler world
    displayCallback       $=      (       evt (EventDisplay) )
    reshapeCallback       $= Just (\ s -> evt (EventReshape s) )
    motionCallback        $= Just (\ p -> evt (EventMouseMove p) )
    passiveMotionCallback $= Just (\ p -> evt (EventMouseMove p) )
    keyboardMouseCallback $= Just
        (\ key kstate modifiers pos -> evt
            ( case (key, kstate) of
                (MouseButton k, Down) -> EventMouseDown k pos
                (MouseButton k, Up  ) -> EventMouseUp   k pos
                (Char k       , Down) -> EventKeyDown   k pos
                (Char k       , Up  ) -> EventKeyUp     k pos
            )
        )

    addTimerCallback timerIntv (onTimerEvent world)
    mainLoop

    where
    onTimerEvent world = do
        clk <- getClock
        eventHandler world (EventTimer clk)
        postRedisplay Nothing
        addTimerCallback timerIntv (onTimerEvent world)

    eventHandler world event = do
        curState <- readIORef world
        nxtState <- onEvent curState event
        writeIORef world nxtState 

    getClock = do
        TOD a b <- getClockTime
        return (a * 1000 + (b `div` 1000000000))