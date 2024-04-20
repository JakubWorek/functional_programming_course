isPalindrome :: [Char] -> Bool
isPalindrome s = reverse s == s

getElemAtIdx :: Int -> [a] -> a
getElemAtIdx x xs = head (drop x xs)

capitalize :: [Char] -> [Char]
capitalize xs = (toEnum(fromEnum(head xs)-32)::Char) : (drop 1 xs)