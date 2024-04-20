import Data.List

sortDesc :: Ord a => [a] -> [a]
sortDesc xs = reverse (sort (xs))

sortDesc' :: Ord a => [a] -> [a]
sortDesc' xs = (reverse . sort) xs

are2FunsEqAt :: Eq a => (t -> a) -> (t -> a) -> [t] -> Bool
are2FunsEqAt f g xs = and [f x == g x | x <- xs]

are2FunsEqAt' :: Eq a => (t -> a) -> (t -> a) -> [t] -> Bool
are2FunsEqAt' f g xs = all (\x -> f x == g x) xs

infixl 9 >.>
(>.>) :: (a -> b) -> (b -> c) -> (a -> c)
g >.> f = f . g

composeFunList :: [a -> a] -> (a -> a)
composeFunList fs = foldr (.) id fs