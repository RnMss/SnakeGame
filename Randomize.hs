{-# language MultiParamTypeClasses #-}

module Randomize
    ( Rand
    , runRand
    , runRandIO
    , getRandom
    , getRandomR
    ) where

import System.Random
import Control.Monad.State.Class

data Rand a =
    Rand (StdGen -> (a, StdGen))

instance Monad Rand where
    -- return :: a -> Rand a
    return x =
        Rand (\gen -> (x, gen) )

    -- (>>=) :: Rand a -> (a -> Rand b) -> Rand b
    Rand r0 >>= fm1 =
        Rand $ \gen0 ->
            let (x1, gen1) = r0 gen0
                Rand r1 = fm1 x1
            in  r1 gen1

instance MonadState StdGen Rand where
    state = Rand

getRandom :: Rand Int
getRandom = Rand random

getRandomR :: Random a => (a, a) -> Rand a
getRandomR range = Rand (randomR range)

runRand :: Int -> Rand a -> a
runRand seed rand = fst (genRand (mkStdGen seed))
    where Rand genRand = rand

runRandIO :: Rand a -> IO a
runRandIO (Rand f) = getStdRandom f
