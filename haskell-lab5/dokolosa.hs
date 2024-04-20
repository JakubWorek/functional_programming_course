{-# LANGUAGE DeriveFunctor #-}

import System.Environment
import System.IO

-- Przepisz poniższe akcje I/O z użyciem notacji do
-- getLine >>= \l1 -> return (l1 ++ l2) >>= \l2 -> print [l1, l2]

funkcja1 :: IO ()
funkcja = do 
    l1 <- getLine
    l2 <- getLine
    print [l1, l2]

-- Przepisz nastepujące wyrażenie używając operatorów >> i >>=
-- do
--   s <- getLine
--   n <- return 3
--   putStrLn $ show n ++ s

funkcja2 :: IO ()
funkcja2 =  getLine >>= \s -> 
            return 3 >>= \n -> 
            putStrLn $ show n ++ s

-- Przepisz nastepujące wyrażenie używając notacji do
-- getLine >>= \s -> return 3 >>= \n -> putStrLn $ s ++ show n

funkcja3 :: IO ()
funkcja3 = do
    s <- getLine
    n <- return 3
    putStrLn $ s ++ show n

-- Dla zdefiniowanego poniżej typu drzewiastego, uzupełnij 
-- funkcję paths tak, by zwracała listę ścieżek od korzenia w dół.
data Tree a = Node a (Tree a) (Tree a) | Leaf 

paths :: Tree a -> [[a]]
paths Leaf = []
paths (Node a lt rt) = concat $ ([(a:)] `fmap`) <*> (paths [lt, rt])

-- Dla zdefiniowanego wyżej typu drzewiastego, uzupełnij
-- funkcję pathSum tak, by zwracała listę sum ścieżek od korzenia w dół.

pathSum :: Num a => Tree a -> [a]
pathSum Leaf = []
pathSum (Node a lt rt) = concat $ (([a +] `fmap`) <*> (pathSum [lt, rt]))

-- Uzupełnij poniższą implementację klasy typu Functor dla
data Either e a = Left e | Right a -- two constructors

instance Functor Either where
    fmap f (Left e) = Left e
    fmap f (Right a) = Right (f a)

-- Uzupełnij poniższą implementację klasy typu Applicative dla
data [] a = [] | a : [a]

instance Applicative (Applicative a => [a]) where
    pure a = [a]
    fs <*> xs = [f x | f <- fs, x <- xs]

-- Podaj wynik obliczenia poniższego wyrażenia:
-- pure (<) <*> Right 1 <*> pure 3
-- Right True

-- Podaj wynik obliczenia poniższego wyrażenia:
-- (\x y z -> (x==y, z==y)) <$> Right 1 <*> Right 2 <*> Left 2
-- Left 2

-- Podaj wynik obliczenia poniższego wyrażenia:
-- (filter odd) <$> Left [1..3]
-- Left [1,2,3]

-- Podaj wynik obliczenia poniższego wyrażenia:
-- (\x y -> max <$> x <*> y) <$> [Just 1, Just 2] <*> [Just 3, Nothing]
-- [Just 3,Nothing,Just 3,Nothing]

-- Podaj wynik obliczenia poniższego wyrażenia:
-- odd <$> [1..4]
-- [True,False,True,False]

-- Dopasuj typy do wyrażeń:
-- fmap : Functor f => (a -> b) -> f a -> f b
-- (<*) : Applicative f => f a -> f b -> f a
-- pure : Applicative f => a -> f a