newtype Box a = MkBox a deriving Show

instance Applicative Box where
  pure = MkBox
  (MkBox f) <*> w = fmap f w

instance Functor Box where
  fmap f (MkBox x) = MkBox (f x)


newtype MyTriple a = MyTriple (a,a,a) deriving Show

instance Functor MyTriple where
  fmap f (MyTriple (a, b, c)) = MyTriple (f a, f b, f c)

instance Applicative MyTriple where
  pure a = MyTriple (a, a, a)
  (MyTriple (a, b, c)) <*> (MyTriple (d, e, f)) = MyTriple (a d, b e, c f)