sgn :: Int -> Int
sgn n = if n < 0
        then -1
        else if n == 0
                then 0
                else 1

absInt :: Int -> Int
absInt x = if x < 0
           then -x
           else x

min2Int :: (Int, Int) -> Int
min2Int (x, y) = if x >= y
                 then y
                 else x

min3Int :: (Int, Int, Int) -> Int
min3Int (x, y, z) = if min2Int(x,y) == y && min2Int(z,y) == y
                    then y
                    else if min2Int(y,x) == x && min2Int(z,x) == x
                            then x
                            else z

toUpper :: Char -> Char
toUpper a = if (fromEnum a) >= 97 && (fromEnum a) <= 122
            then toEnum ((fromEnum a)-32)::Char
            else if (fromEnum a) >= 65 && (fromEnum a) <= 90
                    then a
                    else '!'

toLower :: Char -> Char
toLower a = if (fromEnum a) >= 65 && (fromEnum a) <= 90
            then toEnum ((fromEnum a)+32)::Char
            else if (fromEnum a) >= 97 && (fromEnum a) <= 122
                    then a
                    else '!'

isDigit :: Char -> Bool
isDigit a = if (fromEnum a) >= 48 && (fromEnum a) <= 57
            then True
            else False

charToNum :: Char -> Int
charToNum a = if (fromEnum a) >= 48 && (fromEnum a) <= 57
              then (fromEnum a)-48
              else 0

romanDigit :: Char -> String
romanDigit a = if (fromEnum a) == 49
              then "I"
              else if (fromEnum a) == 50
                      then "II"
                      else if (fromEnum a) == 51
                              then "III"
                              else if (fromEnum a) == 52
                                      then "IV"
                                      else if (fromEnum a) == 53
                                              then "V"
                                              else if (fromEnum a) == 54
                                                      then "VI"
                                                      else if (fromEnum a) == 55
                                                              then "VII"
                                                              else if (fromEnum a) == 56
                                                                      then "VIII"
                                                                      else if (fromEnum a) == 57
                                                                              then "IX"
                                                                              else "ERROR"
