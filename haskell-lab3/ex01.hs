f1 :: Double -> Double
f1 = \x -> x-2

f2 :: Double -> Double -> Double
f2 = \x y -> sqrt (x^2 + y^2)

f3 :: Integer -> Integer -> Integer -> Double
f3 = \x y z -> sqrt (fromIntegral(x^2 + y^2 + z^2))

f4 :: Integer -> Integer
f4 = \x -> 2*x

f5 :: Integer -> Integer
f5 = \x -> x*2

f6 :: Integer -> Integer
f6 = \x -> (2^x)

f7 :: Integer -> Integer
f7 = \x -> (x^2)

f8 :: Double -> Double
f8 = \x -> 2/x

f9 :: Double -> Double
f9 = \x -> x/3

f10 :: Integer -> Integer
f10 = \x -> 4-x

f11 :: Double -> Double
f11 = \x -> sqrt(x)

f12 :: Integer -> Integer -> Integer
f12 = \x y -> abs(x-y)

f13 :: Double -> Double
f13 = \x -> log(x)

f14 :: p -> p
f14 = \x -> x

f15 :: p1 -> p2 -> p1
f15 = \x y -> x

f16 :: Integer -> Bool
f16 = \x -> x `mod` 2 == 0

f17 :: Double -> Double
f17 = \x -> 2 * (sqrt x )^3 * ( sqrt x + 1)