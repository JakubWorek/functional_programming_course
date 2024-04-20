fst2Eq :: Eq a => [a] -> Bool
fst2Eq (x : y : _) | x == y = True
fst2Eq _                    = False

fst2Div :: (Eq a, Integral a) => [a] -> Bool
fst2Div (x : y : _) | x `mod` y == 0 = True
fst2Div _                            = False

fstDivthr :: (Eq a, Integral a) => [a] -> Bool
fstDivthr (x : y : z : _) | x `mod` y == 0 && y `mod` z == 0 = True
fstDivthr _                                                  = False