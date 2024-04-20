absInt :: Int -> Int
absInt n = case ( n>=0 ) of
          True -> n
          _    -> -n

isItTheAnswer :: String -> Bool
isItTheAnswer n = case ( n == "Love" ) of
            True -> True
            _    -> False

not' :: Bool -> Bool
not' n = case ( n ) of
            True  -> False
            False -> True

or' :: (Bool, Bool) -> Bool
or' (a, b) = case ( a || b ) of
            True -> True
            _    -> False

and' :: (Bool, Bool) -> Bool
and' (a, b) = case ( a && b ) of
            True -> True
            _    -> False

nand' :: (Bool, Bool) -> Bool
nand' (a, b) = case ( a && b ) of
            True -> False
            _    -> True

xor' :: (Bool, Bool) -> Bool
xor' (a, b) = case ( a || b ) of
            True -> False
            _    -> True