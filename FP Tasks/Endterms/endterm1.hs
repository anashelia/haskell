{-# LANGUAGE ParallelListComp #-}

main :: IO ()

data Team = Team {teamName :: String, teamID :: Int}

data Player = Player {playerID :: Int, playerName :: String, playerTeam :: Team, playerScores :: [Int]}

data Univ = Univ {uniName :: String, uniID :: Int} deriving (Show, Eq)

data Student = Student {neptunID :: Int, studentName :: String, uni :: Univ, grades :: [Int]} deriving (Show)



--1
mulAdA :: [Int] -> [Int] -> [Int] -> [Int]
mulAdA lst1 lst2 lst3 = [ n1 + (n2 * n3) | n1 <- lst1 | n2 <- lst2 | n3 <- lst3 ]

-- main = print $ mulAdA [1, 2, 3] [4, 5, 6] [7, 8, 9]    -- [29,42,57]
-- main = print $ mulAdA [1, 0, 3] [4, 0, 6] [7, 8, 0]    -- [29,0,3]
-- main = print $ mulAdA [1, -2, 3] [-4, 5, -6] [7, -8, 9]    -- [-27,-42,-51]
-- main = print $ mulAdA [0, 0, 0] [0, 0, 0] [0, 0, 0]    -- [0,0,0]
-- main = print $ mulAdA [1, 2] [3, 4, 5] [6, 7, 8]    -- [19,30]

--2
elementWise :: [Double] -> [Double] -> ([Double], [Double])
elementWise lst1 lst2 = (products, divisions)
    where
        products  = [ n1 * n2 | n1 <- lst1 | n2 <- lst2 ]
        divisions = [ n1 / n2 | n1 <- lst1 | n2 <- lst2, n2 /= 0]

-- main = print (elementWise [1.0, 2.0, 3.0] [4.0, 5.0, 6.0])
-- ([4.0, 10.0, 18.0], [0.25, 0.4, 0.5])
-- main = print (elementWise [1.0, 2.0, 3.0, 4.0] [0.0, 0.0, 0.0, 2.0, 0.0, 3.0, 4.0, 5.0, 6.0])
-- ([0.0,0.0,0.0,8.0],[0.5,0.6666666666666666,0.75,0.8])  
-- main = print (elementWise [] [1.0, 2.0, 3.0]) 
-- ([], [])

--3
processTuples :: [(Int, Bool)] -> [(Int, Bool)] -> Int
processTuples lst1 lst2 = sum [ if b1 && b2 then n1 + n2 else n1 - n2
                               | (n1, b1) <- lst1
                               | (n2, b2) <- lst2 ]



--4
renameUniversity :: Univ -> String -> [Student] -> [Student]
renameUniversity _ _ [] = []
renameUniversity targetUni newName (x : xs)
    | uniName targetUni == uniName (uni x) =
        x { uni = (uni x) { uniName = newName } } : renameUniversity targetUni newName xs
    | otherwise = x : renameUniversity targetUni newName xs

-- main = print $ renameUniversity univ3 "Corvinus" [student6]
-- [Student {neptunID = 106, studentName = "Frank", uni = Univ {uniName = "Corvinus", uniID = 3}, grades = [85,89,84]}]

--5
-- outstandingStudents :: [Student] -> [String]
-- outstandingStudents [] = [] 
-- outstandingStudents (x : xs)
--     | hasHigh (grades x)  = studentName x : outstandingStudents xs
--     | otherwise           = outstandingStudents xs 
--         where 
--             hasHigh [] = false
--             hasHigh (g : gs)
--                 | g > 90    = true  
--                 | otherwise = hasHigh gs

-- outstandingStudents :: [Student] -> [String]
-- outstandingStudents [] = []
-- outstandingStudents (x:xs)
--     | any (>90) (grades x) = studentName x : outstandingStudents xs
--     | otherwise            = outstandingStudents xs


outstandingStudents :: [Student] -> [String]
outstandingStudents [] = [] 
outstandingStudents (x : xs)
    | [ g | g <- grades x, g > 90] /= []    = studentName x : outstandingStudents xs 
    | otherwise                             = outstandingStudents xs


students = [student1, student2, student3, student4, student5, student6]
-- main = print(outstandingStudents students) -- ["Bob","David"]

--6
data Tree a = Node a (Tree a) (Tree a)
            | Leaf
            deriving (Show)

-- this swaps everything
-- swapSubTree :: Tree a -> Tree a
-- swapSubTree Leaf = Leaf
-- swapSubTree (Node x left right) = Node x (swapSubTree right) (swapSubTree left)

data Tree a = Node a (Tree a) (Tree a)
            | Leaf
            deriving (Show)

swapSubTree :: Tree a -> Tree a
swapSubTree (Node a left right) = (Node a right left)

-- main = print $ swapSubTree (Node 99 (Node 3 Leaf Leaf) (Node (-2) Leaf Leaf))
-- Node 99 (Node (-2) Leaf Leaf) (Node 3 Leaf Leaf)


data ExpressionTree a = Operand a | Operator a (ExpressionTree a) (ExpressionTree a) deriving (Show)

evaluate :: ExpressionTree String -> String
evaluate (Operand a) = a
evaluate (Operator a left right) = "( " ++  evaluate left ++ a ++ evaluate right ++ " )"

-- main = print $ evaluate (Operator "+" (Operator "*" (Operand "10") (Operand "9")) (Operator "*" (Operator "-" (Operand "3") (Operand "4")) (Operand "5"))) 
-- "((10*9)+((3-4)*5))"

data Color = Red | Blue | Yellow | Purple | Orange | Green
    deriving (Show, Eq)

combineColors :: Color -> Color -> Color
combineColors c1 c2 | c1 == c2  = c1
combineColors Red Blue     = Purple
combineColors Blue Red     = Purple
combineColors Red Yellow   = Orange
combineColors Yellow Red   = Orange
combineColors Blue Yellow  = Green
combineColors Yellow Blue  = Green
combineColors _ _          = error "Unsupported combo"

-- main = print $ combineColors Red Blue -- Purple


instance Show Team where
    show (Team name tid) =
        "Team {teamName = \"" ++ name ++ "\", teamID = " ++ show tid ++ "}"

instance Show Player where
    show (Player pID pN pT pS) = "Player {ID: " ++ show pID ++ ", Name: \"" ++ pN ++ "\"" ++ ", Team: " ++ show pT ++ ", Scores: " ++ show pS ++ "}"

-- main = print [player1, player2]


instance Eq Player where
    (Player iD1 n1 t1 s1) == (Player iD2 n2 t2 s2)  = (teamName t1 /= teamName t2) && (sum s1 == sum s2)

-- main = print $ player1 == player2 -- False








































team1 = Team {teamName = "Team A", teamID = 1}

team2 = Team {teamName = "Team B", teamID = 2}

team3 = Team {teamName = "Team C", teamID = 3}

team0 = Team {teamName = "Team C", teamID = 3}

player1 = Player {playerID = 1, playerName = "Alice", playerTeam = team1, playerScores = [10, 20, 30]}

player2 = Player {playerID = 2, playerName = "Bob", playerTeam = team1, playerScores = [15, 25, 35]}

player3 = Player {playerID = 3, playerName = "Charlie", playerTeam = team2, playerScores = [12, 22, 32]}

player4 = Player {playerID = 4, playerName = "David", playerTeam = team2, playerScores = [18, 28, 38]}

player5 = Player {playerID = 5, playerName = "Eve", playerTeam = team3, playerScores = [14, 24, 34]}

player6 = Player {playerID = 6, playerName = "Frank", playerTeam = team3, playerScores = [16, 26, 36]}

player7 = Player {playerID = 6, playerName = "Frank", playerTeam = team3, playerScores = [30, 20, 10]}

univ1 = Univ {uniName = "University A", uniID = 1}

univ2 = Univ {uniName = "University B", uniID = 2}

univ3 = Univ {uniName = "University C", uniID = 3}

student1 = Student {neptunID = 101, studentName = "Alice", uni = univ1, grades = [85, 90, 78]}

student2 = Student {neptunID = 102, studentName = "Bob", uni = univ1, grades = [88, 92, 80]}

student3 = Student {neptunID = 103, studentName = "Charlie", uni = univ2, grades = [75, 85, 82]}

student4 = Student {neptunID = 104, studentName = "David", uni = univ2, grades = [90, 87, 91]}

student5 = Student {neptunID = 105, studentName = "Eve", uni = univ3, grades = [80, 83, 88]}

student6 = Student {neptunID = 106, studentName = "Frank", uni = univ3, grades = [85, 89, 84]}

























