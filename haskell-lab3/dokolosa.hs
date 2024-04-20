import Data.List
import Data.Char (isUpper)

--------------------
a :: Integer
a = sum [(x+1)^3 | x <- [1..10], 2*x < 13, even x]

-- przepisz używając funkcji wyższego rzędu
a' :: Integer
a' = foldl (+) 0 . map ((^3) . (+1)) . filter even . filter ((<13) . (2*)) $ [1..10]

--------------------

-- napisz funkcje która dla przekazanego zdania liczy sumę kwadratów
-- długości słów które zaczynają się na "K" i składają z samych dużych liter
fun1 :: String -> Int
fun1 = sum . 
      map ((^2) . length). 
      filter (all isUpper) . 
      filter ((=='K') . head) . 
      words

--------------------
f :: Num t => (t -> a) -> t -> [a]
f g x = [g x, g (x+1), g (x+2)]

-- przepisz używając funkcji wyższego rzędu
f' :: (Integer -> b) -> Integer -> [b]
f' = \g -> map g . (\x -> map ($ x) [id, (+1), (+2)])

--------------------

-- napisz funkcje która dla przekazanego zdania liczy ilość słów
-- posiadających więcej niz dwie samogłoski

fun2 :: String -> Int
fun2 =  length . 
        filter ((>2) . length . filter (`elem` "aeiouyAEIOUY")) . 
        words

--------------------

-- dane jest wyrażenie używajace list comprehension
a1 :: Integer
a1 = sum [x^2 | x <- [1,3..15], x `mod` 3 == 1] 

-- przepisz używając funkcji wyższego rzędu operujących na listach
a1' :: Integer
a1' = foldl (+) 0 .
      map (^2) .
      filter ((==1) . (`mod` 3)) .
      map(+1) . map (*2) $ [0..7]

--------------------

-- foldr (++) ['a'] . map show . filter even $ [n | n <-] -- "2468a"
-- foldl1 (||) . map even . map (\f -> f 1 ) $ [(4^), (+2), const 2, id] -- True
-- foldr (+) 1 . map ((+1) . (*2)) . filter even $ [0..5] -- 16
-- foldl1 (&&) . map even . map (\f -> f 1) $ [ (4^), (+2), const 2, id ] -- False
-- foldr1 (+) . map (sum . (\x -> [x,1]) . ($ 1)) $ zipWith (.) [(2*), (3*)] [(1+), (2+)] -- 15
-- map (sum . (\x -> [x,1]) . ($ 1)) $ zipWith (.) [(2*), (3*)] [(1+), (2+)] -- [5,10]

--------------------

myfun :: (Int, Int) -> Int
myfun (x,y) = (c+d)*(c*d)*(1+c^2+d^2) where (c,d) = (x^2+y, x+y^2)

myfun' = \(x::Int,y::Int) -> let (c,d) = (x^2+y, x+y^2) in (c+d)*(c*d)*(1+c^2+d^2) :: Int

--------------------