fib :: (Eq t, Num t) => t -> t
fib n = 
  if n == 0 || n == 1 then n
  else fib (n-1) + fib (n-2)

fib2 :: Num a => Int -> a
fib2 n  | n <= 0 = 0
        | otherwise = last(take (n+1) fibs)
          where fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

sum' :: Num a => [a] -> a
sum' []   = 0
sum' (x:xs) = x + sum' xs

prod' :: Num a => [a] -> a
prod' [x] = x
prod' (x:xs) = x * prod' xs 

length' :: [a] -> Int
length' [] = 0
length' (x:xs) = 1 + length' xs

or' :: [Bool] -> Bool
or' [x] = x
or' (x:xs) = x || or' xs

and' :: [Bool] -> Bool
and' [x] = x
and' (x:xs) = x && and' xs

elem' :: Eq a => a -> [a] -> Bool
elem' y [x] = y == x
elem' y (x:xs) = x == y || elem' y xs

doubleAll :: Num t => [t] -> [t]
doubleAll [] = []
doubleAll (x:xs) = [x*2] ++ doubleAll xs

squareAll :: Num t => [t] -> [t]
squareAll [] = []
squareAll (x:xs) = [x^2] ++ squareAll xs

selectEven :: Integral t => [t] -> [t]
selectEven [] = []
selectEven (x:xs) = if x `mod` 2 == 0 then [x] ++ selectEven xs
                    else selectEven xs