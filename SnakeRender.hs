module SnakeRender where

import Graphics.Rendering.OpenGL
import Data.Array
import Control.Monad

import SnakeGame
import Utils

renderGame :: SnakeGame -> IO ()
renderGame game = do
    let m0 = game .> gameMap
    let points = indices m0
    let (sx, sy) = game .> mapSize
    let sx' = fromIntegral sx :: Float
    let sy' = fromIntegral sy :: Float

    preservingMatrix $ do
        translate (Vector3 (-1) (1) (0) :: Vector3 GLfloat)
        scale (2.0/sx') (-2.0/sy') 0

        forM_ points (\pos@(x, y) -> do
            let x' = fromIntegral x :: Float
            let y' = fromIntegral y :: Float

            preservingMatrix $ do
                translate (Vector3 x' y' 0)

                case (m0 ! pos) of
                    Snake _ -> drawSnake
                    Food    -> drawFood
                    _       -> return ()
            )

drawBox =
    renderPrimitive LineLoop $ do
        color (Color3 0.5 0.1 0.2 :: Color3 GLfloat)
        mapM_ vertex (
            [ Vertex2 (-0.00) (-0.00)
            , Vertex2 ( 0.95) (-0.00)
            , Vertex2 ( 0.95) ( 0.95)
            , Vertex2 ( 0.00) ( 0.95) :: Vertex2 GLfloat
            ] )

drawSnake =
    renderPrimitive Quads $ do
        color (Color3 1.0 1.0 1.0 :: Color3 GLfloat)
        mapM_ vertex (
            [ Vertex2 0    0
            , Vertex2 0.95 0
            , Vertex2 0.95 0.95
            , Vertex2 0    0.95 :: Vertex2 GLfloat
            ] )

drawFood =
    renderPrimitive Triangles $ do
        color (Color3 0.0 0.7 0.4 :: Color3 GLfloat)
        mapM_ vertex (
            [ Vertex2 ( 0.5 ) ( 0.95)
            , Vertex2 ( 0.15) ( 0.15)
            , Vertex2 ( 0.85) ( 0.15) :: Vertex2 GLfloat
            ] )

