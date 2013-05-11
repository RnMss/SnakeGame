module Utils
    ( (.>)
    )
where

infixl 9 .>

(.>) :: a -> (a -> b) -> b
x .> y = y x
