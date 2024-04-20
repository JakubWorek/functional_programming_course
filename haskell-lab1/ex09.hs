roots :: (Double, Double, Double) -> (Double, Double)
roots (a, b, c) = ( (-b - d) / e, (-b + d) / e )
   where d = sqrt (b * b - 4 * a * c)
         e = 2 * a

unitVec2D :: (Double, Double) -> (Double, Double)
unitVec2D (a, b) = ( a / d, b / d)
    where d = sqrt( a^2 + b^2 )

unitVec3D :: (Double, Double, Double) -> (Double, Double, Double)
unitVec3D (a, b, c) = ( a / d, b / d, c / d )
    where d = sqrt( a^2 + b^2 + c^2 )

triangleArea :: (Double, Double, Double) -> Double
triangleArea (a, b, c) = sqrt(p*(p-a)*(p-b)*(p-c))
    where p = (1/2)*(a+b+c)