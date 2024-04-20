----------------

data MyInt = MkMyInt Int
-- z newtype działa tak samo

instance Eq MyInt where
  (==) (MkMyInt i1) (MkMyInt i2) = i1 == i2

instance Ord MyInt where
  (<=) (MkMyInt i1) (MkMyInt i2) = i1 <= i2

instance Num MyInt where
  (+) (MkMyInt i1) (MkMyInt i2) = MkMyInt (i1 + i2)
  (-) (MkMyInt i1) (MkMyInt i2) = MkMyInt (i1 - i2)
  (*) (MkMyInt i1) (MkMyInt i2) = MkMyInt (i1 * i2)
  negate (MkMyInt i)            = MkMyInt (negate i)
  abs (MkMyInt i)               = MkMyInt (abs i)
  signum (MkMyInt i)            = MkMyInt (signum i)
  fromInteger int               = MkMyInt (fromInteger int)

instance Show MyInt where
  show (MkMyInt i) = "MkMyInt " ++ show i

----------------

data BinTree a =  EmptyBT | 
                  NodeBT a (BinTree a) (BinTree a) 

instance Eq a => Eq (BinTree a) where
    (==) (NodeBT a lt rt) (NodeBT b lt1 rt1) = a==b && lt==lt1 && rt== rt1
    (==) EmptyBT EmptyBT = True
    (==) EmptyBT _ = False
    (==) _ EmptyBT = False

instance Ord a => Ord (BinTree a) where
    (<=) (NodeBT a lt rt) (NodeBT b lt1 rt1) = a<=b && lt<=lt1 && rt<= rt1
    (<=) EmptyBT EmptyBT = True
    (<=) EmptyBT _ = True
    (<=) _ EmptyBT = False

instance Show a => Show (BinTree a) where
    show (NodeBT a lt rt) = "NodeBT " ++ show a ++ " " ++ show lt ++ " " ++ show rt
    show EmptyBT = "EmptyBT"

----------------