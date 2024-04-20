-- Uzupełnij poniższą definicję tak by była poprawna
data Foo a = MkFoo {value :: a, name :: String }

instance Show a => Show (Foo a) where
    show :: Show a => Foo a -> String
    show (MkFoo value name) = "MkFoo " ++ show value ++ " " ++ show name

-- Dal typu drzewiastego o definicji
data Tree a = Node (Tree a) a (Tree a) 
              | Leaf a

-- Napisz funkcję, która zwraca liste elementów drzewa tak,
-- by dla każdego węzła elementy z lewego poddrzewa występowały 
-- w liście przed elementami z prawego poddrzewa

toList :: Tree a -> [a]
toList (Leaf a) = [a]
toList (Node left a right) = toList left ++ [a] ++ toList right

-- Napisz funkcję, która wylicza sumę kwadratów wartości elementów 
-- drzewa
sumSq :: Num a => Tree a -> a
sumSq (Leaf a) = 0
sumSq (Node left a right) = sumSq left + a*a + sumSq right


-- Uzupełnij poniższą definicję tak, by była poprawna
newtype Box a = MkBox {valueInside :: a}

instance Show a => Show (Box a) where
    show :: Show a => Box a -> String
    show (MkBox v) = "MkBox " ++ show v

-- Uzupełnij poniższą definicję tak, by definiowała rekord 
-- z dwoma polami typu String
data Record = Record {first :: String, second :: String}
