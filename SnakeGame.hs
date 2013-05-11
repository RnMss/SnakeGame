module SnakeGame
    ( SnakeGame
    , SnakeState ( Air, Food, Snake )
    , SnakeOperation ( U, D, L, R )
    , gameMap
    , mapSize
    , dead
    , nextSnakeState
    , randSnakeGame
    , Rand
    ) where
 
import Data.Array
import Data.Maybe
import System.Random
import Utils
import Randomize

{-
 - Constants of the game
 -}
snakeGrowth = 3 :: Int

data SnakeState = Air | Food | Snake Int
    
isFood, isSnake, isAir :: SnakeState -> Bool

isSnake (Snake _) = True
isSnake _         = False

isFood Food = True
isFood _    = False

isAir Air = True
isAir _   = False

data SnakeOperation = U | D | L | R
 
data SnakeGame = SnakeGame
    { gameMap :: Array (Int,Int) SnakeState
    , mapSize :: (Int,Int)
    , headPos :: (Int,Int)
    , snakeLen:: Int
    , ticks   :: Int
    , dead    :: Bool
    }

randSnakeGame :: (Int, Int) -> Rand SnakeGame
randSnakeGame size@(width, height) = do
    let x0 = width `div` 3
    let x1 = width - x0 - 1
    let y0 = height `div` 3
    let y1 = height - y0 - 1
    xs <- getRandomR (x0, x1)
    ys <- getRandomR (y0, y1)
    (xf, yf) <- genNeq (xs,ys) ((x0,x1),(y0,y1)) 
    return $ newSnakeGame size (xs, ys) (xf, yf)

genNeq :: (Int,Int) -> ((Int,Int),(Int,Int)) -> Rand (Int,Int)
genNeq p@(x, y) r@((a0,a1), (b0,b1)) = do
    a <- getRandomR (a0, a1)
    b <- getRandomR (b0, b1)
    if (a, b) /= p
        then return (a, b)
        else genNeq p r

newSnakeGame size@(width, height) pos@(x, y) food@(x1, y1)= 
    SnakeGame
        { headPos =
                if (0,0)<=pos && pos<size then
                    pos
                else 
                    error "Out of map!"
        , mapSize = size
        , ticks   = 0
        , snakeLen= 3
        , gameMap =
                let w = width - 1
                    h = height - 1
                in ( 
                    array ((0,0), (w,h))
                        [ ( t
                          , if t==pos then 
                                Snake 0
                            else if t==food then
                                Food
                            else
                                Air
                          )
                        | t <- range ((0,0), (w,h))
                        ] 
                )
        , dead = False
        }

operateOffset :: (Int, Int) -> SnakeOperation -> (Int, Int)
operateOffset (x, y) L = (x-1, y)
operateOffset (x, y) R = (x+1, y)
operateOffset (x, y) U = (x, y-1)
operateOffset (x, y) D = (x, y+1)

nextSnakeState :: 
    SnakeGame -> SnakeOperation -> Rand SnakeGame

nextSnakeState game oper =
    if game .> dead then
        return game
    else
        if collideWall || collideSnake then
            return $ game { 
                dead = True
            }
        else if collideFood then do
            nextMap <- mapMoveSnake m0 1
            return $ game { 
                snakeLen = game.>snakeLen + snakeGrowth,
                gameMap = nextMap,
                ticks = time,
                headPos = nextPos
            }
        else do
            nextMap <- mapMoveSnake m0 0
            return $ game {
                gameMap = nextMap,
                ticks = time,
                headPos = nextPos
            }

    where
    pos    @(px, py) = game .> headPos
    nextPos@(nx, ny) = operateOffset pos oper  
    size   @(sx, sy) = game .> mapSize 
    
    time = (game .> ticks) + 1
    m0 = game .> gameMap

    collideWall = not (inRange (bounds m0) nextPos)
    collideSnake= isSnake (m0!nextPos)
    collideFood = isFood (m0!nextPos)

    newFood :: Rand (Int, Int)
    newFood = do
        randX <- getRandomR (0, sx-1)
        randY <- getRandomR (0, sy-1)
        let foodPos = (randX, randY)
        if isAir (m0!foodPos) && foodPos /= nextPos
            then return foodPos
            else newFood

    mapMoveSnake m foodCnt = do
        diffs <- diffsRand
        return (m // diffs)
        where
        update i@(x,y) point foods
            | i == nextPos  = Just (i, Snake time)
            | elem i foods  = Just (i, Food)
            | otherwise     =
                case point of
                    Snake t0 -> if t0 > time - (game.>snakeLen)  
                                    then Nothing
                                    else Just (i, Air)
                    _        -> Nothing

        diffsRand = do
            foods <- sequence (replicate foodCnt newFood)
            return $ catMaybes 
                [ update i (m!i) foods | i <- indices m ]



