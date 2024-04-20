concat' :: [[a]] -> [a]
concat' []     = []
concat' (x:xs) = x ++ concat' xs

concat'' :: [[a]] -> [a]
concat'' xs = foldr (\x acc -> x ++ acc) [] xs