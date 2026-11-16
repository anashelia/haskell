main :: IO ()

data Employee = Employee {
    employeeID :: Int,
    companyName :: String,
    programmer :: Bool
} deriving (Show)

emp1 :: Employee
emp1 = Employee 123 "Epam" True

emp2 :: Employee
emp2 = Employee {employeeID = 456, companyName = "Apple", programmer = False }

-- main = print $ emp1
-- main = print $ emp2


isProgrammer :: Employee -> String
isProgrammer emp 
    | programmer emp == True    = "Yes"
    | otherwise                 = "No"

-- main = print $ isProgrammer emp1 -- "Yes"
-- main = print $ isProgrammer emp2 -- "No"

changeCompanyName :: Employee -> Employee
changeCompanyName emp 
    | programmer emp == True    = emp { companyName = "X"}
    | otherwise                 = emp

-- main = print $ changeCompanyName emp2 -- Employee {employeeID = 333, companyName = "Twitter", programmer = False}
-- main = print $ changeCompanyName (Employee 111 "Twitter" True) -- Employee {employeeID = 111, companyName = "X", programmer = True}

data Q = Q {nom :: Int, den :: Int} deriving (Show)

simplify :: Q -> Q
simplify Q {nom = n, den = d}
   | d == 0 = error " denominator is 0"
   | d < 0 = Q { nom = (-n) `div` g, den = (-d) `div` g}
   | otherwise = Q { nom = n `div` g, den = d `div` g}
      where g = gcd n d

mkQ :: Int -> Int -> Q
mkQ n d = simplify (Q n d)

equal :: Q -> Q -> Bool
equal (Q n1 d1) (Q n2 d2) 
    | n1 * d2 == d1 * n2     = True
    | otherwise              = False

smallerQ :: Q -> Q -> Bool
smallerQ (Q n1 d1) (Q n2 d2) = n1 * d2 < n2 * d1

plusQ :: Q -> Q -> Q
plusQ (Q n1 d1) (Q n2 d2) = simplify result
    where result = Q (n1 * d2 + n2 * d1) (d1 * d2)

decrementQ :: Q -> Q -> Q
decrementQ (Q n1 d1) (Q n2 d2) = simplify (Q (n1 * d2 - n2 * d1) (d1 * d2))

timesQ :: Q -> Q -> Q
timesQ (Q n1 d1) (Q n2 d2) = simplify (Q (n1*n2) (d1*d2))

divideQ :: Q -> Q -> Q
divideQ (Q n1 d1) (Q n2 d2) = simplify result  
    where result = Q (n1*d2) (d1*n2)

absoluteQ :: Q -> Q
absoluteQ (Q n d)
    | n >= 0 && d > 0 = Q n d
    | n < 0 && d < 0 = Q (-n) (-d)
    | n < 0 && d > 0 = Q (-n) d
    | n > 0 && d < 0 = Q n (-d)
    | n == 0         = Q 0 1

negateQ :: Q -> Q
negateQ (Q n d) = Q (-n) d

integerToQ :: Int -> Q
integerToQ n = Q n 1

rationaltoInt :: Q -> Int
rationaltoInt (Q n d) = n `div` d

isIntQ :: Q -> Bool
isIntQ (Q n d) = n `mod` d == 0

rationaltoReal :: Q -> Float
rationaltoReal (Q n d) = fromIntegral n / fromIntegral d

signOfQ :: Q -> Int
signOfQ (Q n d)
    | n == 0          = 0
    | (n > 0 && d > 0) || (n < 0 && d < 0) = 1
    | otherwise       = -1

q0 = Q { nom = 0, den = 1 }
q1 = Q { nom = 1, den = 1 }
q2 = Q { nom = 1, den = 2 }
q3 = Q { nom = 3, den = 4 }

-- main = print $ simplify (mkQ 81 90) -- Q {nom = 9, den = 10}
-- main = print $ mkQ 81 90 -- Q {nom = 9, den = 10}
-- main = print $ equalQ (mkQ 9 10) (mkQ 81 90) -- True
-- main = print $ smallerQ q2 q3 -- True
-- main = print $ plusQ q2 q0 -- Q {nom = 0, den = 1}
-- main = print $ decrementQ q0 q3 -- Q {nom = 0, den = 1}
-- main = print $ timesQ q0 q2 -- Q {nom = 0, den = 1}
-- main = print $ divideQ q1 q2 -- Q {nom = 2, den = 1}
-- main = print $ absoluteQ q2 -- Q {nom = 1, den = 2}
-- main = print $ signOfQ q2 -- 1
-- main = print $ negateQ q2 -- Q {nom = -1, den = 2}
-- main = print $ integerToQ 4 -- Q {nom = 4, den = 1}
-- main = print $ rationaltoInt q2 -- 0
-- main = print $ isIntQ q1 -- True
-- main = print $ rationaltoReal q2 -- 0.5


data Point = Point {
    x :: Float,
    y :: Float,
    visible :: Bool
} deriving (Show)

isTriangle :: Point -> Point -> Point -> Bool
isTriangle p1 p2 p3 
    | ((x p1) * (y p2 - y p3) + (x p2) * (y p3 - y p1) + (x p3) * (y p1 - y p2)) /= 0 && (visible p1 == True && visible p2 == True && visible p3)  == True   = True
    | otherwise = False
    
origo = Point {x = 0.0, y = 0.0, visible = True}
point1 = Point {x = 0.0, y = 3.0, visible = True}
point2 = Point {x = 2.0, y = 0.0, visible = True}

-- main = print $ isTriangle origo point1 point2 -- True

data Person = Person {
    name :: String, 
    height :: Double
} deriving (Show)

changeHeight :: Person -> Person
changeHeight p1 
    | height p1 > 1.70    = p1 {height = ((height p1) - (height p1 * 0.01))}
    | otherwise           = p1

john :: Person
john = Person {name = "John", height = 1.78}

mike :: Person
mike = Person {name = "Mike", height = 1.58}

lily :: Person
lily = Person {name = "Lily", height = 1.85}

-- main =    print $ changeHeight john  -- Person1 "John" 1.7622
-- main =    print $ changeHeight mike  -- Person1 "Mike" 1.58
-- main =    print $ changeHeight lily  -- Person1 "Lily" 1.8315000000000001


data Student = Student {
    neptunID :: Int,
    uni :: String,
    grades :: [Int]
} deriving (Show)

moreThanNGrades :: [Student] -> Int -> [(Int, String)]
moreThanNGrades lst n = [(neptunID std, uni std) | std <- lst, length(grades std) > n]

students :: [Student]
students = [Student {neptunID=1,uni="Elte",grades=[]}, Student {neptunID=2,uni="BME",grades=[5,5,5]}, Student {neptunID=3,uni="Corvinus",grades=[5,5,5,5]}]

-- main = print $ moreThanNGrades students 1 -- [(2,"BME"),(3,"Corvinus")]
-- main = print $ moreThanNGrades students 3 -- [(3,"Corvinus")]

getAvgGrade :: Student -> Float
getAvgGrade std = fromIntegral (sum (grades std)) / fromIntegral (length (grades std))

-- main = print $ getAvgGrade (students!!1)

getBestStudents :: [Student] -> [Student]
getBestStudents []  = []
getBestStudents lst = [s | s <- lst, getAvgGrade s == maxAvg]
    where maxAvg = maximum (map getAvgGrade lst)

-- main = print $ getBestStudents students -- [Student {neptunID = 2, uni = "BME", grades = [5,5,5]},Student {neptunID = 3, uni = "Corvinus", grades = [5,5,5,5]}]
-- main = print $ getBestStudents [ Student 99 "ELTE" [1,2,3] , Student 101 "BGE" [90,0,45], Student 202 "Corvinus" [50,60], Student 4 "ELTE" [0,25,50,75,100]] -- [Student {neptunID = 202, uni = "Corvinus", grades = [50,60]}]

data APlayer = APlayer {
    playerName :: String,
    skillLevel :: Int
}

shouldWePlay :: [APlayer] -> APlayer -> Bool
shouldWePlay lst goalkeeper = length([ playerName player | player <- lst, skillLevel player >= skillLevel goalkeeper]) >= 3

-- main = print $  shouldWePlay [ APlayer {playerName = "kareem", skillLevel = 4}, APlayer {playerName = "Tarek", skillLevel = 3}, APlayer {playerName = "Ali", skillLevel = 3},APlayer {playerName="Hussien", skillLevel=2}, APlayer {playerName="Ziad", skillLevel=4}] (APlayer {playerName="Gemy", skillLevel=4}) 
-- False
-- main = print $  shouldWePlay [ APlayer {playerName = "kareem", skillLevel = 5}, APlayer {playerName = "Tarek", skillLevel = 4}, APlayer {playerName = "Ali", skillLevel = 3},APlayer {playerName="Hussien", skillLevel=2}, APlayer {playerName="Ziad", skillLevel=4}] (APlayer {playerName="Gemy", skillLevel=4}) 
-- True

data Country = Country {
    countryName :: String,
    capital :: String
} deriving (Show)
 
data Continent = Continent {
    contName :: String,
    countries :: [Country]
} deriving (Show)

isPrime :: Int -> Bool
isPrime n
    | n <= 1    = False
    | otherwise = checkDiv 2
  where
    checkDiv :: Int -> Bool
    checkDiv i
        | i == n        = True           
        | n `mod` i == 0 = False         
        | otherwise     = checkDiv (i+1)

checkI :: Country -> Bool
checkI country1 = length([chars | chars <- countryName country1, chars == 'i']) /= 0
-- checkI country1 = 'i' `elem` capital country1      - this works too 

-- es arasworia 
-- checkCount :: Continet -> Bool
-- checkCount continent1 = map isPrime continent1countries
--     where continent1countries = ([countriess | countriess <- countries continent1, checkI countriess])

checkCount :: Continent -> Bool
checkCount (Continent _ []) = False
checkCount (Continent _ (c:cs))
    | isPrime (length [ch | ch <- capital c, ch == 'i']) = True
    | otherwise = checkCount (Continent "" cs)

continentsPrimeI :: [Continent] -> [String]
continentsPrimeI [] = []
continentsPrimeI (cont:rest)
    | checkCount cont = contName cont : continentsPrimeI rest
    | otherwise       = continentsPrimeI rest

-- Sample countries
macedonia = Country "Macedonia" "Skopje"
hungary = Country "Hungary" "Budapest"
spain = Country "Spain" "Madrid"
brazil = Country "Brazil" "Brasilia"
chile = Country "Chile" "Santiago"
argentina = Country "Argentina" "Buenos Aires"
china = Country "China" "Beijing"
india = Country "India" "New Delhi"
 
-- Sample continents
europe = Continent "Europe" [macedonia, hungary, spain]
asia = Continent "Asia" [china, india]
southAmerica = Continent "South America" [argentina, brazil, chile]

-- main = print $ continentsPrimeI [europe, asia] -- ["Asia"]
-- main = print $ continentsPrimeI [europe]  -- []
-- main = print $ continentsPrimeI [europe, southAmerica, asia] -- ["South America","Asia"]






