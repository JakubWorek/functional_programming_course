data BinIntTree = EmptyIntBT |
                  IntNodeBT Int BinIntTree BinIntTree

sumBinIntTree :: BinIntTree -> Int
sumBinIntTree EmptyIntBT = 0
sumBinIntTree (IntNodeBT n lt rt) = n + sumBinIntTree lt + sumBinIntTree rt

----------------

data BinTree a =  EmptyBT |
                  NodeBT a (BinTree a) (BinTree a)

sumBinTree :: (Num a) => BinTree a -> a
sumBinTree EmptyBT = 0
sumBinTree (NodeBT n lt rt) = n + sumBinTree lt + sumBinTree rt

----------------

data Expr a = Lit a |
              Add (Expr a) (Expr a) |
              Sub (Expr a) (Expr a) 

eval :: Num a => Expr a -> a
eval (Lit n) = n
eval (Add e1 e2) = eval e1 + eval e2
eval (Sub e1 e2) = eval e1 - eval e2

show' :: Show a => Expr a -> String
show' (Lit n) = show n
show' (Add e1 e2) = "(" ++ show' e1 ++ "+" ++ show' e2 ++ ")"
show' (Sub e1 e2) = "(" ++ show' e1 ++ "-" ++ show' e2 ++ ")"

----------------

depthOfBT :: BinTree a -> Int -- głębokość drzewa binarnego
depthOfBT EmptyBT = 0
depthOfBT (NodeBT _ lt rt) = 1 + max (depthOfBT lt) (depthOfBT rt)

flattenBT :: BinTree a -> [a] -- spłaszczenie drzewa binarnego do listy pre-order
flattenBT EmptyBT = []
flattenBT (NodeBT n lt rt) = [n] ++ flattenBT lt ++ flattenBT rt

flattenBT' :: BinTree a -> [a] -- spłaszczenie drzewa binarnego do listy in-order
flattenBT' EmptyBT = []
flattenBT' (NodeBT n lt rt) = flattenBT' lt ++ [n] ++ flattenBT' rt

flattenBT'' :: BinTree a -> [a] -- spłaszczenie drzewa binarnego do listy post-order
flattenBT'' EmptyBT = []
flattenBT'' (NodeBT n lt rt) = flattenBT'' lt ++ flattenBT'' rt ++ [n]

mapBT :: (a -> b) -> BinTree a -> BinTree b -- funkcja map dla drzewa binarnego
mapBT _ EmptyBT = EmptyBT
mapBT f (NodeBT n lt rt) = NodeBT (f n) (mapBT f lt) (mapBT f rt)

insert :: Ord a => a -> BinTree a -> BinTree a -- wstawianie elementu do drzewa binarnego
insert n EmptyBT = NodeBT n EmptyBT EmptyBT
insert n (NodeBT x lt rt)
  | n == x = NodeBT x lt rt
  | n < x = NodeBT x (insert n lt) rt
  | n > x = NodeBT x lt (insert n rt)

list2BST :: Ord a => [a] -> BinTree a -- budowanie drzewa binarnego z listy
list2BST [] = EmptyBT
list2BST (x:xs) = insert x (list2BST xs)

occurs :: Eq a => a -> BinTree a -> Int -- liczba wystąpień elementu w drzewie binarnym
occurs _ EmptyBT = 0
occurs n (NodeBT x lt rt)
  | n == x = 1 + occurs n lt + occurs n rt
  | otherwise = occurs n lt + occurs n rt

elemOf :: Eq a => a -> BinTree a -> Bool -- sprawdzenie czy element należy do drzewa binarnego
elemOf _ EmptyBT = False
elemOf n (NodeBT x lt rt)
  | n == x = True
  | otherwise = elemOf n lt || elemOf n rt

reflect :: BinTree a -> BinTree a -- odbicie lustrzane drzewa binarnego
reflect EmptyBT = EmptyBT
reflect (NodeBT n lt rt) = NodeBT n (reflect rt) (reflect lt)

minElemOf :: Ord a => BinTree a -> a -- minimalny element drzewa binarnego
minElemOf EmptyBT = error "Empty tree"
minElemOf (NodeBT n EmptyBT _) = n
minElemOf (NodeBT _ lt _) = minElemOf lt

maxElemOf :: Ord a => BinTree a -> a -- maksymalny element drzewa binarnego
maxElemOf EmptyBT = error "Empty tree"
maxElemOf (NodeBT n _ EmptyBT) = n
maxElemOf (NodeBT _ _ rt) = maxElemOf rt

foldBinTree :: (a -> b -> b -> b) -> b -> BinTree a -> b -- funkcja fold dla drzewa binarnego
foldBinTree _ acc EmptyBT = acc
foldBinTree f acc (NodeBT n lt rt) = f n (foldBinTree f acc lt) (foldBinTree f acc rt)

mapBT' :: (a -> b) -> BinTree a -> BinTree b -- funkcja map dla drzewa binarnego z wykorzystaniem foldBinTree
mapBT' f = foldBinTree (\n lt rt -> NodeBT (f n) lt rt) EmptyBT

zipBT :: BinTree a -> BinTree b -> BinTree (a, b) -- funkcja zip dla drzewa binarnego
zipBT EmptyBT EmptyBT = EmptyBT
zipBT (NodeBT n1 lt1 rt1) (NodeBT n2 lt2 rt2) = NodeBT (n1, n2) (zipBT lt1 lt2) (zipBT rt1 rt2)

----------------

data GTree a =  Leaf a |
                GNode [GTree a]
                deriving Show

sumGTree :: Num a => GTree a -> a -- suma elementów drzewa ogólnego
sumGTree (Leaf n) = n
sumGTree (GNode []) = 0
sumGTree (GNode (x:xs)) = sumGTree x + sumGTree (GNode xs)

elemOfGTree :: Eq a => a -> GTree a -> Bool -- sprawdzenie czy element należy do drzewa ogólnego
elemOfGTree n (Leaf x) = n == x
elemOfGTree n (GNode []) = False
elemOfGTree n (GNode (x:xs)) = elemOfGTree n x || elemOfGTree n (GNode xs)

depthOfGT :: GTree a -> Int -- głębokość drzewa ogólnego
depthOfGT (Leaf _) = 1
depthOfGT (GNode []) = 0
depthOfGT (GNode (x:xs)) = 1 + max (depthOfGT x) (depthOfGT (GNode xs))

mapGTree :: (a -> b) -> GTree a -> GTree b -- funkcja map dla drzewa ogólnego
mapGTree f (Leaf n) = Leaf (f n)
mapGTree f (GNode []) = GNode []
mapGTree f (GNode (x:xs)) = GNode (mapGTree f x : map (mapGTree f) xs)

flattenGTree :: GTree a -> [a] -- spłaszczenie drzewa ogólnego do listy
flattenGTree (Leaf n) = [n]
flattenGTree (GNode []) = []
flattenGTree (GNode (x:xs)) = flattenGTree x ++ flattenGTree (GNode xs)

countGTreeLeaves :: GTree a -> Int -- liczba liści drzewa ogólnego
countGTreeLeaves (Leaf _) = 1
countGTreeLeaves (GNode []) = 0
countGTreeLeaves (GNode (x:xs)) = countGTreeLeaves x + countGTreeLeaves (GNode xs)

----------------

data Expr' a = Lit' a |
              Expr a :+: Expr a |
              Expr a :-: Expr a

eval' :: Num a => Expr' a -> a
eval' (Lit' n) = n

show'' :: Show a => Expr' a -> String
show'' (Lit' n) = show n
show'' (e1 :+: e2) = "(" ++ show' e1 ++ "+" ++ show' e2 ++ ")"
show'' (e1 :-: e2) = "(" ++ show' e1 ++ "-" ++ show' e2 ++ ")"

----------------