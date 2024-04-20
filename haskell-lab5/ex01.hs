gtc :: IO Char
gtc = getChar

line :: IO String
line = getLine

------------------

newtype IntBox = MkIntBox Int deriving Show
ib1 :: IntBox
ib1 = MkIntBox 1
ib2 :: IntBox
ib2 = MkIntBox 2

------------------