absInt :: Int -> Int
absInt n  | n >= 0    = n
          | otherwise = -n

sgn :: Int -> Int
sgn n | n < 0     = -1
      | n == 0    = 0
      | otherwise = 1

min3Int :: (Int, Int, Int) -> Int
min3Int (x,y,z) | x <= y && x <= z = x
                | y <= x && y <= z = y
                | otherwise        = z

toUpper :: Char -> Char
toUpper a | (fromEnum a) >= 97 && (fromEnum a) <= 122 = toEnum ((fromEnum a)-32)::Char
          | (fromEnum a) >= 65 && (fromEnum a) <= 90  = a
          | otherwise                                 = '!'

toLower :: Char -> Char
toLower a | (fromEnum a) >= 65 && (fromEnum a) <= 90  = toEnum ((fromEnum a)+32)::Char
          | (fromEnum a) >= 97 && (fromEnum a) <= 122 = a
          | otherwise                                 = '!'

isDigit :: Char -> Bool
isDigit a | (fromEnum a) >= 48 && (fromEnum a) <= 57 = True
          | otherwise                                = False

charToNum :: Char -> Int
charToNum a | (fromEnum a) >= 48 && (fromEnum a) <= 57 = (fromEnum a)-48
            | otherwise                                = 0

romanDigit :: Char -> String
romanDigit a | (fromEnum a) == 49 = "I"
             | (fromEnum a) == 50 = "II"
             | (fromEnum a) == 51 = "III"
             | (fromEnum a) == 52 = "IV"
             | (fromEnum a) == 53 = "V"
             | (fromEnum a) == 54 = "VI"
             | (fromEnum a) == 55 = "VII"
             | (fromEnum a) == 56 = "VIII"
             | (fromEnum a) == 57 = "IX"
             | otherwise          = "ERROR"