import Data.Char

doubleElems :: Num a => [a] -> [a]
doubleElems [] = []
doubleElems (x:xs) = 2 * x : doubleElems xs

sqrElems :: Num a => [a] -> [a]
sqrElems [] = []
sqrElems (x:xs) = x^2 : sqrElems xs

lowerCase :: String -> String
lowerCase [] = []
lowerCase (x:xs) = toLower x : lowerCase xs

map' :: (a -> b) -> [a] -> [b]
map' f [] = []
map' f (x:xs) = (f x) : map' f xs

doubleElems' :: Num a => [a] -> [a]
doubleElems' = map' (\x -> 2 * x)

sqrElems' :: Num a => [a] -> [a]
sqrElems' = map' (\x -> x^2)

lowerCase' :: String -> String
lowerCase' = map' (\x -> toLower x)

map'' :: (a -> b) -> [a] -> [b]
map'' f xs = [f x | x <- xs]

doubleElems'' :: [Integer] -> [Integer]
doubleElems'' = map'' (\x -> 2*x)
sqrElems'' :: [Integer] -> [Integer]
sqrElems''   = map'' (\x -> x^2)
lowerCase'' :: [Char] -> [Char]
lowerCase''  = map'' (\x -> toLower x)