---------------

class Mappable t where
  fMap :: (a -> b) -> t a -> t b

data Vec3D a = Vec3D {x::a, y::a, z::a} deriving Show

instance Mappable Vec3D where
  fMap f (Vec3D x y z) = Vec3D (f x) (f y) (f z)

---------------

newtype Pair a = Pair (a,a) deriving Show

instance Mappable Pair where
  fMap f (Pair (x,y)) = Pair (f x, f y)

---------------

data BinTree a =  EmptyBT | 
                  NodeBT a (BinTree a) (BinTree a)
                  deriving Show

instance Mappable BinTree where
  fMap f EmptyBT = EmptyBT
  fMap f (NodeBT a lt rt) = NodeBT (f a) (fMap f lt) (fMap f rt)

instance Mappable Maybe where
  fMap f Nothing = Nothing
  fMap f (Just a) = Just (f a)

instance Mappable Either where
  fMap f (Left a) = Left (f a)
  fMap f (Right a) = Right (f a)

instance Mappable ((->) a) where
  fMap f g = f . g

---------------

class VectorLike t where
  (|==|) :: Eq a => t a -> t a -> Bool
  (|+|), (|-|) :: (Num a) => t a -> t a -> t a
  (|*|) :: (Num a) => t a -> t a -> a
  (||?), (|-?) :: (Num a, Eq a) => t a -> t a -> Bool -- równoległość i prostopadłość
  vectLength :: Floating a => t a -> a
  unitVectOf :: Floating a => t a -> t a

data Vec2D a = Vec2D {a::a, b::a} deriving Show

instance VectorLike Vec2D where
  (|==|) (Vec2D a b) (Vec2D c d) = a==c && b==d
  (|+|) (Vec2D a b) (Vec2D c d) = Vec2D (a+c) (b+d)
  (|-|) (Vec2D a b) (Vec2D c d) = Vec2D (a-c) (b-d)
  (|*|) (Vec2D a b) (Vec2D c d) = a*c + b*d
  (||?) (Vec2D a b) (Vec2D c d) = a*d - b*c == 0
  (|-?) (Vec2D a b) (Vec2D c d) = a*c + b*d == 0
  vectLength (Vec2D a b) = sqrt (a*a + b*b)
  unitVectOf (Vec2D a b) = Vec2D (a / vectLength (Vec2D a b)) (b / vectLength (Vec2D a b))

data Vec3D a = Vec3D {a::a, b::a, c::a} deriving Show

instance VectorLike Vec3D where
  (|==|) (Vec3D a b c) (Vec3D d e f) = a==d && b==e && c==f
  (|+|) (Vec3D a b c) (Vec3D d e f) = Vec3D (a+d) (b+e) (c+f)
  (|-|) (Vec3D a b c) (Vec3D d e f) = Vec3D (a-d) (b-e) (c-f)
  (|*|) (Vec3D a b c) (Vec3D d e f) = a*d + b*e + c*f
  (||?) (Vec3D a b c) (Vec3D d e f) = (Vec3D a b c) |*| (Vec3D d e f) == 0
  (|-?) (Vec3D a b c) (Vec3D d e f) = (Vec3D a b c) |*| (Vec3D d e f) == 0
  vectLength (Vec3D a b c) = sqrt (a*a + b*b + c*c)
  unitVectOf (Vec3D a b c) = Vec3D (a / vectLength (Vec3D a b c)) (b / vectLength (Vec3D a b c)) (c / vectLength (Vec3D a b c))

---------------