{-# LANGUAGE Arrows, RecursiveDo #-}

module Circuit
    ( Circuit
    , runCircuit, runCircuits
    , regC, delayC, bufferC, foldC
    , CircuitT
    , runCircuitT, runCircuitT' )
where

import Control.Arrow
import Control.Category
import Prelude hiding (id, (.))
 
newtype Circuit a b = Circuit 
    { runCircuit :: a -> (b, Circuit a b) }

data CircuitT a b r =
    CircuitT ( (Circuit a b) -> (r, Circuit a b) )

instance Monad (CircuitT i o) where
    -- return :: r -> CircuitT i o r
    return r = CircuitT $ \a -> (r, a)

    -- (>>=) :: CircuitT i o r0 
    --       -> (r0 -> CircuitT i o r1) 
    --       -> CircuitT i o r1
    CircuitT t >>= f =
        CircuitT $ \c0 ->
            let (r0, c1) = t c0
                CircuitT t1 = f r0
            in  t1 c1

feed :: a -> CircuitT a b b
feed a = CircuitT $ \c -> runCircuit c a

runCircuitT :: CircuitT a b r -> Circuit a b -> (r, Circuit a b)
runCircuitT (CircuitT t) c = t c

runCircuitT' = flip runCircuitT

{-
 - Example:
     
    sum :: Circuit Int Int
    sum = foldC 0 (+)

    getSum3 a b c = 
        let (r, endC) = 
            runCircuitT' sum $ do
                feed a
                feed b
                x <- feed c
                return x
        in r
 -}

instance Category Circuit where
    id = Circuit (\a -> (a, id))

    (Circuit c2) . (Circuit c1) =
        Circuit $ \a0 ->
            let (a1, c1') = c1 a0
                (a2, c2') = c2 a1
            in  (a2, c2' . c1')
 
instance Arrow Circuit where
    arr f = cir where
        cir = Circuit (\a -> (f a, cir))

    first (Circuit c) = 
        Circuit $ \(a1, a2) ->
            let (b1, c') = c a1
            in  ((b1, a2), first c')

    -- The following are not nessesary to defined an Arrow,
    -- however, defining them could make it run faster.
    second (Circuit c) = 
        Circuit $ \(a1, a2) ->
            let (b2, c') = c a2
            in  ((a1, b2), second c')

    (Circuit c1) *** (Circuit c2) =
        Circuit $ \(a1, a2) ->
            let (b1, c1') = c1 a1
                (b2, c2') = c2 a2
            in  ((b1, b2), c1' *** c2')

    (Circuit c1) &&& (Circuit c2) =
        Circuit $ \a->
            let (b1, c1') = c1 a
                (b2, c2') = c2 a
            in  ((b1, b2), c1' &&& c2')

instance ArrowLoop Circuit where
    {-|
     -|         +-----------------------+
     -|         |                       |
     -|         |     .-----------.     |
     -|   (a)--->----->  Circuit  >----->----(b)
     -|         |  .-->     c     >--.  |
     -|         |  |  `-----------'  |  |
     -|         |  `-------(t)-------'  |
     -|         |                       |
     -|         +-----------------------+
     -|              loop (Circuit c)
     -}
    loop (Circuit c) =
        Circuit $ \a ->
            let ((b, t), c') = c (a, t)
            in  (b, loop c')

runCircuits :: Circuit a b -> [a] -> [b]
runCircuits c [] = []
runCircuits c (a:as) = 
    let (b, c') = runCircuit c a
    in  b : runCircuits c' as

regC :: a -> Circuit (Maybe a) a
regC def =
    Circuit $ \maybeX -> 
        case maybeX of
            Nothing -> (def, regC def)
            Just x  -> (x  , regC x)

delayC :: a -> Circuit a a
delayC x = Circuit $ \y -> (x, delayC y)

bufferC :: [a] -> Circuit a a
bufferC [] = id :: Circuit a a
bufferC (a:as) = Circuit $ \y -> (a, bufferC (as ++ [y]))

foldC :: s -> (a -> s -> s) -> Circuit a s
foldC initial f = 
    proc input -> do
        rec let x1 = f input x0
            x0 <- delayC initial -< x1
        id -< x1

trigger :: Circuit a b -> Circuit (Maybe a) (Maybe b)
trigger cir = Circuit run' where
    run' (Just a) = (Just b, trigger cir')
    run' Nothing  = (Nothing, trigger cir')
    (b, cir') = runCircuit cir a

stateToCircuit :: (MonadState m) => (a -> m b) -> (m b -> b) -> Circuit a b
stateToCircuit action run =
    Circuit $ \a -> 
        let (b, n) = run $ do
                b <- action a
                n <- get
                return (b, n)
            run' m = run $! put n >> m
        in  (b, stateToCircuit action run')
