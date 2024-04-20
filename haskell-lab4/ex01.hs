polarToCartesian :: Floating a => (a, a) -> (a, a)
polarToCartesian (r, phi) = (r * cos phi, r * sin phi)

type CartesianCoord' a = (a, a)
type PolarCoord' a = (a, a)

polarToCartesian' :: Floating a => PolarCoord' a -> CartesianCoord' a
polarToCartesian' (r, phi) = (r * cos phi, r * sin phi)

newtype CartesianCoord'' a = MkCartesianCoord'' (a, a)
newtype PolarCoord'' a = MkPolarCoord'' (a, a)

polarToCartesian'' :: Floating a => PolarCoord'' a -> CartesianCoord'' a
polarToCartesian''  (MkPolarCoord'' (r, phi)) = 
                    MkCartesianCoord'' (r * cos phi, r * sin phi)

------------------

personInfoToString :: (String, String, String) -> String
personInfoToString (nm, snm, addr) = 
    "name: " ++ nm ++ ", surname: " ++ snm ++ ", addr: " ++ addr

type Name' = String
type Surname' = String
type Address' = String
type PersonInfo' = (Name', Surname', Address')
type PersonInfoToStringType = PersonInfo' -> String

personInfoToString' :: PersonInfoToStringType
personInfoToString' (nm, snm, addr) = 
    "name: " ++ nm ++ ", surname: " ++ snm ++ ", addr: " ++ addr

newtype Name'' = MkName'' (String)
newtype Surname'' = MkSurname'' (String)
newtype Address'' = MkAddress'' (String)
newtype PersonInfo'' = MkPersonInfo'' (Name'', Surname'', Address'')
newtype PersonInfoToStringType'' = MkPersonInfoToStringType'' (PersonInfo'' -> String)

personInfoToString'' :: PersonInfo'' -> String
personInfoToString'' (MkPersonInfo'' (MkName'' nm, MkSurname'' snm, MkAddress'' addr)) = 
    "name: " ++ nm ++ ", surname: " ++ snm ++ ", addr: " ++ addr

------------------