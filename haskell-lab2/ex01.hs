myFun :: Num a => a -> a
myFun x = 2 * x

add2T :: Num a => (a, a) -> a
add2T (x,y) = x + y

add2C :: Num a => (a -> (a -> a))
add2C x y = x + y

add3T :: Num a => (a, a, a) -> a
add3T (x,y,z) = x + y + z


add3C :: Num a => (a -> (a -> (a -> a)))
add3C x y z = x + add2C y z 