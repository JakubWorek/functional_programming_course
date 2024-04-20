-- Uzupełnij poniższą implementację kalsy typu Eq dla następujących danych:
data T1 a b = B (a,b) | A -- dwa konstruktory

instance (Eq a, Eq b) => Eq (T1 a b) where
  (==) :: (Eq a, Eq b) => T1 a b -> T1 a b -> Bool
  (==) (B (a1,b1)) (B (a2,b2)) = a1 == a2 && b1 == b2
  (==) A A = True
  (==) _ _ = False

-- Algebraiczny typ danych w Haskellu
-- to taki, który jest zbudowany z innych typów przez użycie dwóch operacji
-- określonych na poziomie typu: dodawania i mnożenia.

-- Dla nastepującego typu danych (drzewo BST)
data BST a = EmptyBST | NodeBST a (BST a) (BST a) -- dwa konstruktory

-- uzupełnij poniższą definicję funkcji isElemOdBST (sprawdza, czy element jest w drzewie)

isElemOfBST :: Ord a => a -> BST a -> Bool
isElemOfBST _ EmptyBST = False
isElemOfBST x (NodeBST e lt rt) 
  | x == e = True
  | x < e = isElemOfBST x lt
  | otherwise = isElemOfBST x rt

-- Dla następującego typu danych (drzewo binarne)
data BinTree a = NodeBT (BinTree a) (BinTree a) | Leaf a -- dwa konstruktory

-- uzupełnij poniższą definicję instancji t1, która jest zrównoważona
-- i przechowuje dwie wartości : 1 i 1
t1 :: BinTree Integer
t1 = NodeBT (Leaf 1) (Leaf 1)

-- Dla następującego typu danych (drzewo binarne)
data BinTree' a = Leaf' a | NodeBT' (BinTree a) (BinTree a) -- dwa konstruktory

-- uzupełnij poniższą deginicję funkcji mapBT (funkcja map dla typu BinTree)
mapBT :: (a -> b) -> BinTree a -> BinTree b
mapBT f (Leaf a) = Leaf (f a)
mapBT f (NodeBT lt rt) = NodeBT (mapBT f lt) (mapBT f rt)

-- Dla następującego typu danych (drzewo binarne)
data BinTree'' a = Leaf'' a | NodeBT'' (BinTree a) (BinTree a) -- dwa konstruktory

-- uzupełnij poniższą definicję funkcji flattenBT (BinTree to list, wersja left first)
flattenBT :: BinTree a -> [a]
flattenBT (Leaf a) = [a]
flattenBT (NodeBT lt rt) = flattenBT lt ++ flattenBT rt

-- Dla nastepującego typu danych (drzewo binarne)
data BTree a = EmtyBT | NodBT (BTree a) a (BTree a) -- dwa konstruktory

-- uzupełnij poniższą definicję instancji t2, która jest zbalansowana i przechowuje trzy wartości 1,1,1
t2 :: BTree Integer
t2 = NodBT (NodBT (EmtyBT) 1 (EmtyBT)) 1 (NodBT (EmtyBT) 1 (EmtyBT))

-- Dla następującego typu danych 
data BST' a = EmptyBST' | NodeBST' (BST' a) a (BST' a) -- dwa konstruktory

-- uzupełnij poniższą definicje funkcji insert2BST (wstawia element do drzewa BST)
insert2BST :: Ord a => a -> BST' a -> BST' a
insert2BST x EmptyBST' = NodeBST' EmptyBST' x EmptyBST'
insert2BST x (NodeBST' lt e rt)
  | x == e = NodeBST' lt e rt
  | x < e = insert2BST x lt
  | otherwise = insert2BST x rt

-- Abstrakcyjny typ danych (ADT) w Haskellu to taki:
-- którego specyfikacja jest ograniczona do związanych z nim operacji
-- z całkowitym pominięciem szczegółów reprezentacji/implementacji.

-- Uzupełnij poniższą definicję implementacji typu Eq dla nastepującego typu danych:
data T2 = T2_C1 (Bool, Double) | T1_C2 -- dwa konstruktory

instance Eq T2 where
  (==) :: T2 -> T2 -> Bool
  (==) (T2_C1 (b1,d1)) (T2_C1 (b2,d2)) = b1 == b2 && d1 == d2
  (==) T1_C2 T1_C2 = True
  (==) _ _ = False

-- Uzupełnij poniższą definicję implementacji klasy typu Eq
-- dla nasteupującego typu danych:
data T3 a b c = C a (b,c) | D (a,b) -- dwa konstruktory

instance (Eq a, Eq b, Eq c) => Eq (T3 a b c) where
  (==) :: (Eq a, Eq b, Eq c) => T3 a b c -> T3 a b c -> Bool
  (==) (C a1 (b1,c1)) (C a2 (b2,c2)) = a1 == a2 && b1 == b2 && c1 == c2
  (==) (D (a1,b1)) (D (a2,b2)) = a1 == a2 && b1 == b2
  (==) _ _ = False

-- Uzupełnij poniższą definicję implementacji klasy typu Eq
-- dla nasteupującego typu danych:
data T4 = T4_C1 Int | T4_C2 Double Bool -- dwa konstruktory

instance Eq T4 where
  (==) :: T4 -> T4 -> Bool
  (==) (T4_C1 i1) (T4_C1 i2) = i1 == i2
  (==) (T4_C2 d1 b1) (T4_C2 d2 b2) = d1 == d2 && b1 == b2
  (==) _ _ = False