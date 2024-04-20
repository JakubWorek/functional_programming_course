sqr :: Double -> Double
sqr x = x * x

vec2DLen :: (Double, Double) -> Double
vec2DLen (x, y) = sqrt (x^2 + y^2)

vec3DLen :: (Double, Double, Double) -> Double
vec3DLen (x, y, z) = sqrt(x^2 + y^2 + z^2)

swap :: (Int, Char) -> (Char, Int)
swap (x, y) = (y, x)

threeEqual :: (Int, Int, Int) -> Bool
threeEqual (x, y, z) = (x+y > z) && (x+z > y) && (y+z > x)

traingleArea :: (Double, Double, Double) -> Double
traingleArea (x, y, z) = sqrt( (1 / 2) * (x + y + z) *  (1 / 2) * (x + y + z) - x) * ( (1 / 2) * (x + y + z) - y ) * ( (1 / 2) * (x + y + z) - z  )