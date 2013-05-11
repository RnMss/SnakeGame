module Randomize
    ( Rand
    , runRand
    , runRandIO
    , getRandom
    , getRandomR
    ) where

import System.Random

data Rand a =
    Rand (StdGen -> (a, StdGen))

instance Monad Rand where
    -- return :: a -> Rand a
    return x =
        Rand (\gen -> (x, gen) )

    -- (>>=) :: Rand a -> (a -> Rand b) -> Rand b
    m1 >>= fm2 = Rand m1_fm2
        where {
            m1_fm2 gen0 = (x2, gen2)
            where {
                Rand f1 = m1;
                (x1, gen1) = f1 gen0;
                Rand f2 = fm2 x1;
                (x2, gen2) = f2 gen1;
            }
        }

getRandom :: Rand Int
getRandom = Rand random

getRandomR :: Random a => (a, a) -> Rand a
getRandomR range = Rand (randomR range)

runRand :: Int -> Rand a -> a
runRand seed rand = fst (genRand (mkStdGen seed))
    where Rand genRand = rand

runRandIO :: Rand a -> IO a
runRandIO (Rand f) = getStdRandom f
