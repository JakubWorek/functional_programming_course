import Data.List

isSortedAsc :: Ord a => [a] -> Bool
isSortedAsc xs = foldl (&&) True (zipWith (==) xs (sort xs))

everySecond :: [t] -> [t]
everySecond xs = map (\(x,y) -> x)  (filter (\(x,y) -> odd y) (zip xs [1..(length xs)]))

zip3' :: [a] -> [b] -> [c] -> [(a,b,c)]
zip3' xs ys zs = map (\((x,y),z) -> (x,y,z)) (zip (zip xs ys) zs)

unzip3' :: [(a,b,c)] -> ([a],[b],[c])
unzip3' xs = (map (\(x,y,z) -> x) xs, map (\(x,y,z) -> y) xs, map (\(x,y,z) -> z) xs)

isSortedDesc :: Ord a => [a] -> Bool
isSortedDesc xs = foldl (&&) True (zipWith (==) xs (reverse (sort xs)))

isSorted :: Ord a => [a] -> Bool
isSorted xs = isSortedAsc xs || isSortedDesc xs

fibs :: [Int]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs) :: [Int]