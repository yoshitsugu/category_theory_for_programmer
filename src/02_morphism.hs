module MorphismA (morphism_) where

addRep_ :: [b] -> [[b]] -> [[b]]
addRep_ [] _ = []
addRep_ _ [] = []
addRep_ (y:ys) a@(l:ls) = (y:l) : addRep_ [y] ls ++ addRep_ ys a


morphism_ :: [a] -> [b] -> [[b]]
morphism_ _ [] = []
morphism_ [] ys = [ys]
morphism_ (x:[]) (y:ys) = [y] : morphism_ (x:[]) ys
morphism_ (x:xs) ys = addRep_ ys $ morphism_ xs ys

morphism :: (Num b, Num a, Enum b, Enum a) => a -> b -> [[b]]
morphism m n = morphism_ [1..m] [1..n]

morphism0 :: (Num a, Enum a) => a -> [[a]]
morphism0 n = morphism 0 n
