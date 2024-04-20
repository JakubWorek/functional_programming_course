trojki :: [(Integer, Integer, Integer)]
trojki = [(a,b,c) | a <- [1..100], b <- [a..100], c <- [b..100], a^2 + b^2 == c^2]

primes :: [Int]
primes = sieve [2..]
  where
    sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p /= 0]

isPrime :: Int -> Bool
isPrime n = n `elem` (take n primes)

primesIn1000 :: Int
primesIn1000 = length (takeWhile (<1000) primes)