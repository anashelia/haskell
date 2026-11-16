main :: IO ()



data Student = Student
  { sName          :: String
  , house          :: String
  , year           :: Int
  , housePoints    :: Int
  , detentions     :: Int
  , playsQuidditch :: Bool
  , isPrefect      :: Bool
  } deriving (Show, Eq)

data School = SLeaf | SNode Student School School deriving (Show, Eq)

-- Sample Students
-- Gryffindor
harry = Student "Harry Potter" "Gryffindor" 5 80 20 True False
hermione = Student "Hermione Granger" "Gryffindor" 5 120 1 False True
ron = Student "Ron Weasley" "Gryffindor" 5 90 3 True True

-- Slytherin
draco = Student "Draco Malfoy" "Slytherin" 5 70 5 True True
crabbe = Student "Vincent Crabbe" "Slytherin" 5 30 7 False False
goyle = Student "Gregory Goyle" "Slytherin" 5 25 6 False False

-- Ravenclaw
cho = Student "Cho Chang" "Ravenclaw" 6 85 1 True False
luna = Student "Luna Lovegood" "Ravenclaw" 4 60 0 False False
padma = Student "Padma Patil" "Ravenclaw" 5 75 0 False True

-- Hufflepuff
ernie = Student "Ernie Macmillan" "Hufflepuff" 5 65 0 False True
hannah = Student "Hannah Abbott" "Hufflepuff" 5 55 0 False False
zacharias = Student "Zacharias Smith" "Hufflepuff" 5 70 2 True False

allStudents :: [Student]
allStudents =
  [ harry, hermione, ron
  , draco, crabbe, goyle
  , cho, luna, padma
  , ernie, hannah, zacharias
  ]


-- 1. activeQuidditchPlayers
activeQuidditchPlayers :: [Student] -> [String]
activeQuidditchPlayers [] = []
activeQuidditchPlayers (x : xs) 
    | playsQuidditch x = sName x : activeQuidditchPlayers xs
    | otherwise        = activeQuidditchPlayers xs

-- main = print (activeQuidditchPlayers [luna, hannah])
-- []

-- main = print (activeQuidditchPlayers allStudents)
-- ["Harry Potter","Ron Weasley","Draco Malfoy","Cho Chang","Zacharias Smith"]


-- 2. prefectsOfHouse
prefectsOfHouse :: [Student] -> String -> [String]
prefectsOfHouse [] _ = []
prefectsOfHouse (x : xs) houseName 
    | (house x == houseName && isPrefect x)   = sName x : prefectsOfHouse xs houseName
    | otherwise                               = prefectsOfHouse xs houseName

-- main = print (prefectsOfHouse allStudents "Gryffindor")
-- ["Hermione Granger","Ron Weasley"]

-- main = print (prefectsOfHouse allStudents "Slytherin")
-- ["Draco Malfoy"]

-- main = print (prefectsOfHouse allStudents "Unknown")
-- Expected: []


-- 3. averageHousePoints
sums :: [Student] -> String -> Int
sums students houseName =
  sum [ housePoints s | s <- students, house s == houseName ]

countStudentsInHouse :: [Student] -> String -> Int
countStudentsInHouse students houseName =
  length [ s | s <- students, house s == houseName ]

averageHousePoints :: [Student] -> String -> Float
averageHousePoints lst houseName 
    | count == 0         = 0.0
    | otherwise          = fromIntegral total / fromIntegral count
    where 
        total = sums lst houseName
        count = countStudentsInHouse lst houseName

-- main = print (averageHousePoints allStudents "Gryffindor")
-- 96.6

-- main = print (averageHousePoints allStudents "Ravenclaw")
-- 73.3

-- main = print (averageHousePoints allStudents "Unknown")
-- 0.0


-- 4. insertStudent
insertStudent :: Student -> School -> School
insertStudent newStd SLeaf = SNode newStd SLeaf SLeaf
insertStudent newStd (SNode std left right )
    | housePoints newStd < housePoints std    = SNode std (insertStudent newStd left) right
    | otherwise                               = SNode std left (insertStudent newStd right)

sampleSchool = insertStudent harry (insertStudent draco (insertStudent hermione SLeaf))
-- main = print sampleSchool


-- 5. countTroubleFree
countTroubleFree :: School -> Int
countTroubleFree SLeaf  = 0
countTroubleFree (SNode std left right) 
    | detentions std == 0      = 1 + countTroubleFree left + countTroubleFree right
    | otherwise                = countTroubleFree left + countTroubleFree right

school5 = insertStudent harry (insertStudent draco (insertStudent hermione SLeaf))
-- main = print (countTroubleFree school5)
-- 0

school5b = insertStudent luna (insertStudent padma (insertStudent ernie SLeaf))
-- main = print (countTroubleFree school5b)
-- 3


-- 6. bonusCandidates
bonusCandidates :: School -> [Student]
bonusCandidates SLeaf = []
bonusCandidates (SNode std left right) 
    | housePoints std >= 80 && detentions std <= 1   = std : (bonusCandidates left ++ bonusCandidates right)
    | otherwise                                      = bonusCandidates left ++ bonusCandidates right

school6 = insertStudent harry (insertStudent draco (insertStudent hermione SLeaf))
-- main = print (bonusCandidates school6)
-- [Student {sName = "Hermione Granger", house = "Gryffindor", year = 5, housePoints = 120, detentions = 1, playsQuidditch = False, isPrefect = True}]

school6b = insertStudent luna (insertStudent padma (insertStudent cho SLeaf))
-- main = print (bonusCandidates school6b)
-- [Student {sName = "Cho Chang", house = "Ravenclaw", year = 6, housePoints = 85, detentions = 1, playsQuidditch = True, isPrefect = False}]

-- main = print (bonusCandidates SLeaf)
-- []


-- the above function printed the entire student, not only their name, so i wrote one more which prints just the name
bonusCandidates1 :: School -> [String]
bonusCandidates1 SLeaf = []
bonusCandidates1 (SNode std left right) 
    | housePoints std >= 80 && detentions std <= 1   = sName std : (bonusCandidates1 left ++ bonusCandidates1 right)
    | otherwise                                      = bonusCandidates1 left ++ bonusCandidates1 right

-- main = print (bonusCandidates1 school6)
-- [Hermione Granger]

-- main = print (bonusCandidates1 school6b)
-- [Cho Chang]

-- main = print (bonusCandidates1 SLeaf)
-- []



