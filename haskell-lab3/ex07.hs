onlyEven :: Integral a => [a] -> [a]
onlyEven [] = []
onlyEven (x:xs)
  | x `mod` 2 == 0 = x : onlyEven xs
  | otherwise      = onlyEven xs

onlyOdd :: Integral a => [a] -> [a]
onlyOdd [] = []
onlyOdd (x:xs)
  | x `mod` 2 == 1 = x : onlyOdd xs
  | otherwise      = onlyOdd xs

onlyUpper :: String -> String
onlyUpper [] = []
onlyUpper (x:xs)
  | x `elem` ['A'..'Z'] = x : onlyUpper xs
  | otherwise           = onlyUpper xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' p [] = []
filter' p (x:xs)
  | p x       = x : filter' p xs
  | otherwise = filter' p xs

onlyEven' :: Integral a => [a] -> [a]
onlyEven' = filter' (\x -> x `mod` 2 == 0)

onlyOdd' :: Integral a => [a] -> [a]
onlyOdd' = filter' (\x -> x `mod` 2 == 1)

onlyUpper' :: String -> String
onlyUpper' = filter' (\x -> x `elem` ['A'..'Z'])