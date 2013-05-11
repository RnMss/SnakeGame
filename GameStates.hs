module GameStates where

import SnakeGame

data GS = GS 
    { input :: !GSInput
    , world :: !SnakeGame
    }

data GSInput = GSInput 
    { keyU :: !Bool
    , keyD :: !Bool
    , keyL :: !Bool
    , keyR :: !Bool
    , lastOperation :: !SnakeOperation
    }

newGSRand :: Rand GS
newGSRand = do
    snakeWorld <- randSnakeGame (20, 20)
    return $ GS {
        input = GSInput {
            keyU = False,
            keyD = False,
            keyL = False,
            keyR = False,
            lastOperation = U
        },
        world = snakeWorld
    }