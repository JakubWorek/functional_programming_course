sum' :: Num a => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

sumSqr' :: Num a => [a] -> a
sumSqr' [] = 0
sumSqr' (x:xs) = x^2 + sumSqr' xs

sumWith :: Num a => (a -> a) -> [a] -> a
sumWith f [] = 0
sumWith f (x:xs) = f x + sumWith f xs

sum :: [Integer] -> Integer
sum = sumWith (\x -> x)

sumSqr :: [Integer] -> Integer
sumSqr = sumWith (\x -> x^2)

sumCube :: [Integer] -> Integer
sumCube = sumWith (\x -> x^3)

sumAbs :: [Integer] -> Integer
sumAbs = sumWith (\x -> abs x)

listLength :: [Integer] -> Integer
listLength = sumWith (\x -> 1)

prod' :: Num a => [a] -> a
prod' [] = 1
prod' (x:xs) = x * prod' xs

prodWith :: Num a => (a -> a) -> [a] -> a
prodWith f [] = 1
prodWith f (x:xs) = f x * prodWith f xs

prod :: [Integer] -> Integer
prod = prodWith (\x -> x)

prodSqr :: [Integer] -> Integer
prodSqr = prodWith (\x -> x^2)

prodCube :: [Integer] -> Integer
prodCube = prodWith (\x -> x^3)

prodAbs :: [Integer] -> Integer
prodAbs = prodWith (\x -> abs x)