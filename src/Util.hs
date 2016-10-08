module Util where

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a, b, c) = f a b c

getOrFail :: Either String a -> a
getOrFail x = case x of
    Right a -> a
    Left e -> error e