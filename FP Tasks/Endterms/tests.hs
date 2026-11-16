-- ----------------------------------------------------------------------------------------------------------------------------------
-- ----------------------------------------------------------------------------------------------------------------------------------
-- ----------------------------------------------------------------------------------------------------------------------------------
-- -- test1--

-- -- WRITE NAME AND NEPTUN CODE HERE

-- {-# LANGUAGE ParallelListComp #-}

-- main :: IO ()

-- data Team = Team {teamName :: String, teamID :: Int}

-- data Player = Player {playerID :: Int, playerName :: String, playerTeam :: Team, playerScores :: [Int]}

-- data Univ = Univ {uniName :: String, uniID :: Int} deriving (Show, Eq)

-- data Student = Student {neptunID :: Int, studentName :: String, uni :: Univ, grades :: [Int]} deriving (Show)

-- team1 = Team {teamName = "Team A", teamID = 1}

-- team2 = Team {teamName = "Team B", teamID = 2}

-- team3 = Team {teamName = "Team C", teamID = 3}

-- team0 = Team {teamName = "Team C", teamID = 3}

-- player1 = Player {playerID = 1, playerName = "Alice", playerTeam = team1, playerScores = [10, 20, 30]}

-- player2 = Player {playerID = 2, playerName = "Bob", playerTeam = team1, playerScores = [15, 25, 35]}

-- player3 = Player {playerID = 3, playerName = "Charlie", playerTeam = team2, playerScores = [12, 22, 32]}

-- player4 = Player {playerID = 4, playerName = "David", playerTeam = team2, playerScores = [18, 28, 38]}

-- player5 = Player {playerID = 5, playerName = "Eve", playerTeam = team3, playerScores = [14, 24, 34]}

-- player6 = Player {playerID = 6, playerName = "Frank", playerTeam = team3, playerScores = [16, 26, 36]}

-- player7 = Player {playerID = 6, playerName = "Frank", playerTeam = team3, playerScores = [30, 20, 10]}

-- univ1 = Univ {uniName = "University A", uniID = 1}

-- univ2 = Univ {uniName = "University B", uniID = 2}

-- univ3 = Univ {uniName = "University C", uniID = 3}

-- student1 = Student {neptunID = 101, studentName = "Alice", uni = univ1, grades = [85, 90, 78]}

-- student2 = Student {neptunID = 102, studentName = "Bob", uni = univ1, grades = [88, 92, 80]}

-- student3 = Student {neptunID = 103, studentName = "Charlie", uni = univ2, grades = [75, 85, 82]}

-- student4 = Student {neptunID = 104, studentName = "David", uni = univ2, grades = [90, 87, 91]}

-- student5 = Student {neptunID = 105, studentName = "Eve", uni = univ3, grades = [80, 83, 88]}

-- student6 = Student {neptunID = 106, studentName = "Frank", uni = univ3, grades = [85, 89, 84]}


-- {-3- 10p
-- Write a function that takes two lists of tuples as input. 
-- Each tuple contains an Integer and a Boolean value. 
-- For each pair of tuples at the same index in the two lists:
-- If the Boolean values in both tuples are True, add the integers from the tuples.
-- Otherwise, subtract the integer in the second tuple from the integer in the first tuple.
-- The function should return the sum of all these computed values.
-- Parallel list comprehension must be used.
-- Non-parallel solutions are also accepted but then max is 8 points.
-- Example: 
-- processTuples [(1, True), (2, False), (6,false)] [(3,True), (4,True), (5,False)]
--         (1, True) (3, True) -> 1+3 = 4
--         (2, False) (4, True) -> 2-4 = -2
--         (6, False) (5, False) -> 6-5 = 1
--         Result = 4 + -2 + 1 = 3
-- -}

-- processTuples :: [(Int,Bool)] -> [(Int, Bool)] -> Int
-- processTuples lst1 lst2 = sum [ if b1 && b2 then x + y else x - y | (x, b1) <- lst1 | (y, b2) <- lst2]


-- -- main = print $ processTuples [(1, True), (2, False), (6,False)] [(3,True), (4,True), (5,False)] -- 3
-- -- main = print $ processTuples [(4, True), (7, True), (1, False)] [(2, True), (5, False), (3, True)] -- 6
-- -- main = print $ processTuples [(0, False), (10, True), (8, False)] [(5, False), (3, True), (2, False)] -- 14
-- -- main = print $ processTuples [(1, True), (3, False), (5, False)] [(1, True), (3, False), (5, True)] -- 2


-- -----------------------------------------------------

-- {-4- 10p
-- A university wants to update its name. Given a university, a new name as a string, 
-- and a list of students, update the university name for all students associated 
-- with that university to the new name.
-- Example: 
-- uni1 "ELTE" [student1, student2, student3] -> 
--  [Student {neptunID = 101, studentName = "Alice", uni = Univ {uniName = "ELTE", uniID = 1}, grades = [85, 90, 78]},
--   Student {neptunID = 102, studentName = "Bob", uni = Univ {uniName = "ELTE", uniID = 1}, grades = [88, 92, 80]},
--   Student {neptunID = 103, studentName = "Charlie", uni = Univ {uniName = "University B", uniID = 2}, grades = [75, 85, 82]}]
--   student1 and student2 have their uniName updated to "ELTE", because they are associated with uni1,
--   student3 remains unchanged.
-- -}

-- renameUniversity :: Univ -> String -> [Student] -> [Student]
-- renameUniversity _ _ [] = []
-- renameUniversity targetUni newName (x : xs)
--     | uniName targetUni == uniName (uni x) =
--         x { uni = (uni x) { uniName = newName } } : renameUniversity targetUni newName xs
--     | otherwise = x : renameUniversity targetUni newName xs

-- --main = print $ renameUniversity univ3 "Corvinus" [student6]
-- -- [Student {neptunID = 106, studentName = "Frank", uni = Univ {uniName = "Corvinus", uniID = 3}, grades = [85,89,84]}]
-- --main = print $ renameUniversity univ1 "ELTE" [student1, student2, student3] 
-- -- [Student {neptunID = 101, studentName = "Alice", uni = Univ {uniName = "ELTE", uniID = 1}, grades = [85,90,78]},
-- --    Student {neptunID = 102, studentName = "Bob", uni = Univ {uniName = "ELTE", uniID = 1}, grades = [88,92,80]},
-- --    Student {neptunID = 103, studentName = "Charlie", uni = Univ {uniName = "University B", uniID = 2}, grades = [75,85,82]}]
-- --main = print $ renameUniversity univ2 "BME" [student3, student4, student6, student5]
-- -- [Student {neptunID = 103, studentName = "Charlie", uni = Univ {uniName = "BME", uniID = 2}, grades = [75,85,82]},
-- --  Student {neptunID = 104, studentName = "David", uni = Univ {uniName = "BME", uniID = 2}, grades = [90,87,91]},
-- --  Student {neptunID = 106, studentName = "Frank", uni = Univ {uniName = "University C", uniID = 3}, grades = [85,89,84]},
-- --  Student {neptunID = 105, studentName = "Eve", uni = Univ {uniName = "University C", uniID = 3}, grades = [80,83,88]}]


-- -----------------------------------------------------

-- {-5- 10p
-- Write a function that lists all students who have at least one grade 
-- greater than 90. Write the names of these students.
-- -}

-- students = [student1, student2, student3, student4, student5, student6]

-- outstandingStudents :: [Student] -> [String]
-- outstandingStudents [] = []
-- outstandingStudents (x:xs)
--     | any (>90) (grades x) = studentName x : outstandingStudents xs
--     | otherwise            = outstandingStudents xs


-- --main = print(outstandingStudents students) -- ["Bob","David"]





----------------------------------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------------------------------
-- test3--
-- WRITE NAME AND NEPTUN CODE HERE

{-# LANGUAGE ParallelListComp #-}

main :: IO ()

data Univ = Univ {uniName :: String, uniID :: Int} deriving (Show, Eq)

data Student = Student {neptunID :: Int, studentName :: String, uni :: Univ, grades :: [Int]} deriving (Show)

univ1 = Univ {uniName = "University A", uniID = 1}

univ2 = Univ {uniName = "University B", uniID = 2}

univ3 = Univ {uniName = "University C", uniID = 3}

student1 = Student {neptunID = 101, studentName = "Alice", uni = univ1, grades = [85, 90, 78]}

student2 = Student {neptunID = 102, studentName = "Bob", uni = univ1, grades = [88, 92, 80]}

student3 = Student {neptunID = 103, studentName = "Charlie", uni = univ2, grades = [75, 85, 82]}

student4 = Student {neptunID = 104, studentName = "David", uni = univ2, grades = [90, 87, 91]}

student5 = Student {neptunID = 105, studentName = "Eve", uni = univ3, grades = [80, 83, 88]}

student6 = Student {neptunID = 106, studentName = "Frank", uni = univ3, grades = [85, 89, 84]}


-- 3. 10p
-- You are given a student, a character, and a number. 
-- If the student's name contains the given character
-- and the university's id matches the given number, 
-- increase the student's neptunID by 100.

rec1 :: Student -> Char -> Int -> Student
rec1 std ch n 
    | (ch `elem` (studentName std)) && (uniID (uni std) == n)   = std{neptunID = (neptunID std) + 100}
    | otherwise = std

-- main = print $ rec1 student1 'c' 1
-- Student {neptunID = 201, studentName = "Alice", uni = Univ {uniName = "University A", uniID = 1}, grades = [85,90,78]}
--main = print $ rec1 student1 'x' 1
-- Student {neptunID = 101, studentName = "Alice", uni = Univ {uniName = "University A", uniID = 1}, grades = [85,90,78]}
--main = print $ rec1 student2 'o' 2
-- Student {neptunID = 102, studentName = "Bob", uni = Univ {uniName = "University A", uniID = 1}, grades = [88,92,80]}
--main = print $ rec1 student2 'o' 1 
-- Student {neptunID = 202, studentName = "Bob", uni = Univ {uniName = "University A", uniID = 1}, grades = [88,92,80]}


-- ************************************************

data Tree a = Node a (Tree a) (Tree a)
            | Leaf
            deriving (Show)

tree1 :: Tree Int
tree1 = Node 0
            (Node 1
                (Node 3 Leaf Leaf)
                (Node 4 Leaf Leaf))
            (Node 2
                (Node 5 Leaf Leaf)
                (Node 6 Leaf Leaf))

tree2 :: Tree Int
tree2 = Node 0
            (Node 1
                (Node 5 Leaf (Node 8 Leaf Leaf))
                Leaf)
            (Node 2 Leaf Leaf)

tree3 :: Tree Int
tree3 = Node 4
            Leaf
            (Node 2
                (Node 1 Leaf Leaf)
                (Node 3 Leaf Leaf))


-- ************************************************

-- 5. 10p
-- Find the left-most node value of a binary tree.
-- For leaves should be error with "tree is empty".

--tr1 :: Tree a -> a

--main = print $ tr1 tree1 -- 3
--main = print $ tr1 tree2 -- 5
--main = print $ tr1 tree3 -- 4
--main = print $ tr1 (Leaf :: Tree Int) -- tree is empty


-- ************************************************

-- 6. 10p
-- You are given a tree and a function.
-- Apply this function to every node of the tree.

--tr2 :: Tree a -> (a -> a) -> Tree a

--main = print $ tr2 tree2 (+100) 
-- Node 100 (Node 101 (Node 105 Leaf (Node 108 Leaf Leaf)) Leaf) (Node 102 Leaf Leaf)
--main = print $ tr2 tree2 (*5) 
-- Node 0 (Node 5 (Node 25 Leaf (Node 40 Leaf Leaf)) Leaf) (Node 10 Leaf Leaf)
--main = print $ tr2 tree3 (\x -> (x `mod` 2) + 50 ) 
-- Node 50 Leaf (Node 50 (Node 51 Leaf Leaf) (Node 51 Leaf Leaf))


-- ************************************************

-- 7. 10p
-- Given a binary tree where each node contains an integer value, 
-- modify the tree as follows: For each node, calculate the sum 
-- of its direct children (left and right).
-- If the sum of the direct children is negative, set the node's value to -1 
-- (meaning update the parent node's value to -1).
-- Otherwise, leave the node's value unchanged.
-- Leaf nodes remain unchanged, as they don't have any children. 
-- Consider leaves' value 0.
--    Example: 
--      10              -1      parent node updated to -1, since -5 + -3 = -8
--     /  \      ->    /  \
--    -5  -3         -5   -3

--changeTree :: Tree Int -> Tree Int

--main = print $ changeTree Leaf -- Leaf
--main = print $ changeTree (Node 3 Leaf Leaf) -- Node 3 Leaf Leaf
--main = print $ changeTree (Node 10 (Node (-5) Leaf Leaf) (Node (-6) Leaf Leaf))
-- Expected Output: Node (-1) (Node (-5) Leaf Leaf) (Node (-6) Leaf Leaf)
--main = print $ changeTree (Node 5 (Node (10) (Node 1 Leaf Leaf) (Node (-1) Leaf Leaf)) (Node 3 Leaf (Node (-8) Leaf Leaf)))
-- Expected Output: Node 5 
--                       (Node (10) 
--                           (Node 1 Leaf Leaf) 
--                           (Node (-1) Leaf Leaf))
--                       (Node (-1)
--                           Leaf 
--                           (Node (-8) Leaf Leaf))


-- ************************************************

-- 8. 10p
-- Implement the Eq instance for an integer Binary Search Tree.
-- Two binary search trees are equal if they have exactly the same node values.

-- instancing for eq 

tree00,tree11,tree22,tree33:: Tree Int
tree00 = Node 10 (Node 5 (Node 3 Leaf Leaf) (Node 7 Leaf Leaf)) (Node 15 (Node 12 (Node 11 Leaf Leaf) Leaf) (Node 20 Leaf Leaf))
tree11 = Node 10 (Node 5 (Node 3 Leaf Leaf) (Node 7 Leaf Leaf)) (Node 15 (Node 12 Leaf Leaf) (Node 20 Leaf Leaf))
tree22 = Node 10 (Node 5 (Node 3 Leaf Leaf) (Node 7 Leaf Leaf)) (Node 15 (Node 12 Leaf Leaf) (Node 20 Leaf Leaf))
tree33 = Node 10 (Node 5 (Node 3 Leaf Leaf) (Node 7 Leaf Leaf)) (Node 12 (Node 11 Leaf Leaf) (Node 15 Leaf (Node 20 Leaf Leaf)))

--main = print $ tree11 == tree22 -- True
--main = print $ tree00 == tree33 -- True
--main = print $ tree11 == tree33 -- False


-- ************************************************

-- 9. 10p
-- Write a custom Show instance for a the data type Sport. 
-- The instance should format the output to display the name of the sport 
-- and its associated information in the form "Sport_Name: <data>".
-- For example:
-- If the sport is Football with the associated data "Played by 22", the output should be "Football: Played by 22".
-- If the sport is Voleyball with the associated data True, the output should be "Voleyball: True".
-- If the sport is Basketball with the associated data 5, the output should be "Basketball: 5".

data Sport a = Football a | Voleyball a | Basketball a 

-- instancing for show 

--main = print $ Football "Played by 22" -- Football: "Played by 22"
--main = print $ Basketball 5 -- Basketball: 5
--main = print $ Voleyball True -- Voleyball: True
--main = print $ Basketball [1,2,3,4] -- Basketball: [1,2,3,4]

-- ************************************************









