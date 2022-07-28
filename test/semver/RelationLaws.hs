module RelationLaws(implies, reflexivity, transitivity) where

implies :: Bool -> Bool -> Bool
implies a b = not a || b

reflexivity :: (a -> a -> Bool) -> a -> Bool
reflexivity f a = f a a

transitivity :: (a -> a -> Bool) -> a -> a  -> a -> Bool
transitivity f a b c = (f a b && f b c) `implies` f a c
