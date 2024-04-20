-- product type example (one constructor)
data CartInt2DVec = MkCartInt2DVec Int Int 
-- konwencja: prefix 'Mk' dla konstruktora

xCoord :: CartInt2DVec -> Int
xCoord (MkCartInt2DVec x _) = x

yCoord :: CartInt2DVec -> Int
yCoord (MkCartInt2DVec _ y) = y

----------------

data Cart2DVec' a = MkCart2DVec' a a

xCoord' :: Cart2DVec' a -> a
xCoord' (MkCart2DVec' x _) = x

yCoord' :: Cart2DVec' a -> a
yCoord' (MkCart2DVec' _ y) = y

----------------

data Cart2DVec'' a = MkCart2DVec'' {x::a, y::a}

--xCoord'' :: Cart2DVec'' a -> a
--xCoord'' (MkCart2DVec'' {x = xVal, y = _}) = xVal

--yCoord'' :: Cart2DVec'' a -> a
--yCoord'' (MkCart2DVec'' {y = yVal, x = _}) = yVal 

----------------

-- sum type example (two constructors)
data List a = EmptyL | Cons a (List a) deriving Show

head' :: List a -> a
head' EmptyL      = error "head': the empty list has no head!"
head' (Cons x xs) = x

----------------

-- enum type example (special case of sum type) 
data ThreeColors =  Blue |
                    White |
                    Red

type ActorName = String

leadingActor :: ThreeColors -> ActorName
leadingActor Blue  = "Juliette Binoche"
leadingActor White = "Zbigniew Zamachowski"
leadingActor Red   = "Irene Jacob"

----------------

{-
uwaga: ta sama nazwa* dla:
  - konstruktora typu (po lewej)
  - konstruktora danych/wartości (po prawej)

  * druga (obok omówionej poprzednio -- z prefiksem 'Mk') powszechna konwencja w Haskellu!
-}

data Cart3DVec a = Cart3DVec a a a

xCoord3D :: Cart3DVec a -> a
xCoord3D (Cart3DVec x _ _) = x

yCoord3D :: Cart3DVec a -> a
yCoord3D (Cart3DVec _ y _) = y

zCoord3D :: Cart3DVec a -> a
zCoord3D (Cart3DVec _ _ z) = z

----------------

data Cart3DVec' a = Cart3DVec' {x' :: a, y' :: a, z' :: a}

----------------

data Shape =  Circle Float |
              Rectangle Float Float

area :: Shape -> Float
area (Circle r)      = pi * r^2
area (Rectangle a b) = a * b

----------------

data Tree a = EmptyT |
              Node a (Tree a) (Tree a) 
              deriving Show

rootValue :: Tree a -> a
rootValue EmptyT    = error "rootValue: the empty tree has no root!"
rootValue (Node n _ _) = n

----------------

data TrafficLights = RedLight | YellowLight | GreenLight

actionFor :: TrafficLights -> String
actionFor RedLight    = "Stop!"
actionFor YellowLight = "Prepare!"
actionFor GreenLight  = "Go!"

----------------