qSort :: Ord a => [a] -> [a]
qSort [] = []
qSort (x:xs) = qSort (leftPart xs) ++ [x] ++ qSort (rightPart xs)
  where
    leftPart  xs = [ y | y <- xs, y <= x ]
    rightPart xs = [ y | y <- xs, y > x  ]

qSort' :: Ord a => [a] -> [a]
qSort' [] = []
qSort' (x:xs) = qSort' (leftPart xs) ++ [x] ++ qSort' (rightPart xs)
  where
    leftPart  xs = filter (<=x) xs
    rightPart xs = filter (>x) xs

mSort :: Ord a => [a] -> [a]
mSort [] = []
mSort [x] = [x]
mSort xs = merge (mSort left) (mSort right)
  where
    (left, right) = splitAt (length xs `div` 2) xs
    merge [] ys = ys
    merge xs [] = xs
    merge (x:xs) (y:ys) | x <= y    = x : merge xs (y:ys)
                        | otherwise = y : merge (x:xs) ys

iSort :: Ord a => [a] -> [a]
iSort [] = []
iSort (x:xs) = insert x (iSort xs)
  where
    insert x [] = [x]
    insert x (y:ys) | x <= y    = x : y : ys
                    | otherwise = y : insert x ys

concat' :: [[a]] -> [a]
concat' [] = []
concat' (x:xs) = x ++ concat' xs

concat'' :: [[a]] -> [a]
concat'' [] = []
concat'' (x:xs) = foldr (++) x xs

isSorted :: [Int] -> Bool
isSorted [] = True
isSorted [x] = True
isSorted (x:y:xs) = x <= y && isSorted (y:xs)

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

zip' :: [a] -> [b] -> [(a,b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (z:zs) = [(x,z)] ++ zip' xs zs

unzip' :: [(a,b)] -> ([a],[b])
unzip' [] = ([],[])
unzip' ((x,y):xs) = (x:fst(unzip' xs), y:snd(unzip' xs))

zip3' :: [a] -> [b] -> [c] -> [(a,b,c)]
zip3' [] _ _ = []
zip3' _ [] _ = []
zip3' _ _ [] = []
zip3' (x:xs) (y:ys) (z:zs) = [(x,y,z)] ++ zip3' xs ys zs

subList :: Eq a => [a] -> [a] -> Bool
subList [] _ = True
subList _ [] = False
subList (x:xs) (y:ys) | x == y    = subList xs ys
                      | otherwise = subList (x:xs) ys