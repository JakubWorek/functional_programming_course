f1 :: p -> p
f1 x = x

f2 :: p -> Bool
f2 x = True

f3 :: Num a => (a, a) -> a
f3 (x, y) = x + y

f4 :: Fractional a => (a, a) -> a
f4 (x,y) = x / y

f5 :: Eq a => (a, a) -> Bool
f5 (x,y) = x /= y

f6 :: Ord a => (a, a) -> Bool
f6 (x,y) = x > y

f7 :: (Ord a, Fractional a) => (a, a) -> a
f7 (x,y) = if x > y then x + y else x / 4

f8 :: Eq a => (a, a) -> Bool
f8 (x,y) = x == y

f9 :: (Ord a1, Num a1, Eq a2) => (a1, a2, a1) -> Bool
f9 (x,y,z) = if x + 3 < z
              then True
              else y /= y

collatz :: Integral a => a -> a
collatz n =
  let divides d n = n `mod` d == 0
      isEven n = divides 2 n 
  in  if isEven n 
      then n `div` 2
      else 3 * n + 1     

f10 :: Num p => p -> p
f10 x = let a = 10 * x
        in a
        where a = 100 * x


f11 :: Ord a => (a, a, a) -> (a, a, a)
f11 (x,y,z)   | x>y = (x,y,z)
              | z == y = (z,x,y)
              | otherwise = (y,x,z)