{-# LANGUAGE ParallelListComp #-}
import Data.List
import Data.Char
import Data.Maybe (fromJust)

main :: IO ()
-- main = print "Run individual tests by uncommenting them below."

-- ============================================================================
-- SHARED DATA STRUCTURES & SAMPLES
-- ============================================================================

data Team = Team {teamName :: String, teamID :: Int}
data Player = Player {playerID :: Int, playerName :: String, playerTeam :: Team, playerScores :: [Int]}

data Univ = Univ {uniName :: String, uniID :: Int} deriving (Show, Eq)
data Student = Student {neptunID :: Int, studentName :: String, uni :: Univ, grades :: [Int]} deriving (Show)

data Tree a = Node a (Tree a) (Tree a) | Leaf deriving (Show)

-- Shared Sample Data
team1 = Team {teamName = "Team A", teamID = 1}
team2 = Team {teamName = "Team B", teamID = 2}
team3 = Team {teamName = "Team C", teamID = 3}
team0 = Team {teamName = "Team C", teamID = 3} -- Duplicate name, used for testing Eq

player1 = Player {playerID = 1, playerName = "Alice", playerTeam = team1, playerScores = [10, 20, 30]}
player2 = Player {playerID = 2, playerName = "Bob", playerTeam = team1, playerScores = [15, 25, 35]}
player3 = Player {playerID = 3, playerName = "Charlie", playerTeam = team2, playerScores = [12, 22, 32]}
player4 = Player {playerID = 4, playerName = "David", playerTeam = team2, playerScores = [18, 28, 38]}
player5 = Player {playerID = 5, playerName = "Eve", playerTeam = team3, playerScores = [14, 24, 34]}
player6 = Player {playerID = 6, playerName = "Frank", playerTeam = team3, playerScores = [16, 26, 36]}
player7 = Player {playerID = 6, playerName = "Frank", playerTeam = team3, playerScores = [30, 20, 10]}
player8 = Player {playerID = 8, playerName = "Peter", playerTeam = team2, playerScores = [10, 5, 5]}

univ1 = Univ {uniName = "University A", uniID = 1}
univ2 = Univ {uniName = "University B", uniID = 2}
univ3 = Univ {uniName = "University C", uniID = 3}

student1 = Student {neptunID = 101, studentName = "Alice", uni = univ1, grades = [85, 90, 78]}
student2 = Student {neptunID = 102, studentName = "Bob", uni = univ1, grades = [88, 92, 80]}
student3 = Student {neptunID = 103, studentName = "Charlie", uni = univ2, grades = [75, 85, 82]}
student4 = Student {neptunID = 104, studentName = "David", uni = univ2, grades = [90, 87, 91]}
student5 = Student {neptunID = 105, studentName = "Eve", uni = univ3, grades = [80, 83, 88]}
student6 = Student {neptunID = 106, studentName = "Frank", uni = univ3, grades = [85, 89, 84]}
student7 = Student {neptunID = 103, studentName = "Charlie1", uni = univ2, grades = [75, 59, 82]}
student8 = Student {neptunID = 105, studentName = "Eve1", uni = univ3, grades = [80, 9, 8]}

students = [student1, student2, student3, student4, student5, student6]

tree1 :: Tree Int
tree1 = Node 0 (Node 1 (Node 3 Leaf Leaf) (Node 4 Leaf Leaf)) (Node 2 (Node 5 Leaf Leaf) (Node 6 Leaf Leaf))

tree2 :: Tree Int
tree2 = Node 0 (Node 1 (Node 5 Leaf (Node 8 Leaf Leaf)) Leaf) (Node 2 Leaf Leaf)

tree3 :: Tree Int
tree3 = Node 4 Leaf (Node 2 (Node 1 Leaf Leaf) (Node 3 Leaf Leaf))

-- ============================================================================
-- 1. ENDTERM 1 (endterm1.hs)
-- ============================================================================

-- 1.1
mulAdA :: [Int] -> [Int] -> [Int] -> [Int]
mulAdA lst1 lst2 lst3 = [ n1 + (n2 * n3) | n1 <- lst1 | n2 <- lst2 | n3 <- lst3 ]

-- main = print $ mulAdA [1, 2, 3] [4, 5, 6] [7, 8, 9]    -- [29,42,57]

-- 1.2
elementWise :: [Double] -> [Double] -> ([Double], [Double])
elementWise lst1 lst2 = (products, divisions)
    where
        products  = [ n1 * n2 | n1 <- lst1 | n2 <- lst2 ]
        divisions = [ n1 / n2 | n1 <- lst1 | n2 <- lst2, n2 /= 0]

-- main = print (elementWise [1.0, 2.0, 3.0] [4.0, 5.0, 6.0])
-- ([4.0, 10.0, 18.0], [0.25, 0.4, 0.5])

-- 1.3
processTuples :: [(Int, Bool)] -> [(Int, Bool)] -> Int
processTuples lst1 lst2 = sum [ if b1 && b2 then n1 + n2 else n1 - n2
                               | (n1, b1) <- lst1
                               | (n2, b2) <- lst2 ]

-- 1.4
renameUniversity :: Univ -> String -> [Student] -> [Student]
renameUniversity _ _ [] = []
renameUniversity targetUni newName (x : xs)
    | uniName targetUni == uniName (uni x) =
        x { uni = (uni x) { uniName = newName } } : renameUniversity targetUni newName xs
    | otherwise = x : renameUniversity targetUni newName xs

-- main = print $ renameUniversity univ3 "Corvinus" [student6]

-- 1.5
outstandingStudents :: [Student] -> [String]
outstandingStudents [] = [] 
outstandingStudents (x : xs)
    | [ g | g <- grades x, g > 90] /= []    = studentName x : outstandingStudents xs 
    | otherwise                             = outstandingStudents xs

-- main = print(outstandingStudents students) -- ["Bob","David"]

-- 1.6
swapSubTree :: Tree a -> Tree a
swapSubTree (Node a left right) = (Node a right left)
swapSubTree Leaf = Leaf

-- main = print $ swapSubTree (Node 99 (Node 3 Leaf Leaf) (Node (-2) Leaf Leaf))

data ExpressionTree a = Operand a | Operator a (ExpressionTree a) (ExpressionTree a) deriving (Show)

evaluate :: ExpressionTree String -> String
evaluate (Operand a) = a
evaluate (Operator a left right) = "( " ++  evaluate left ++ a ++ evaluate right ++ " )"

-- main = print $ evaluate (Operator "+" (Operator "*" (Operand "10") (Operand "9")) (Operator "*" (Operator "-" (Operand "3") (Operand "4")) (Operand "5"))) 

data Color = Red | Blue | Yellow | Purple | Orange | Green deriving (Show, Eq)

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
    show (Team name tid) = "Team {teamName = \"" ++ name ++ "\", teamID = " ++ show tid ++ "}"

instance Show Player where
    show (Player pID pN pT pS) = "Player {ID: " ++ show pID ++ ", Name: \"" ++ pN ++ "\"" ++ ", Team: " ++ show pT ++ ", Scores: " ++ show pS ++ "}"

instance Eq Player where
    (Player iD1 n1 t1 s1) == (Player iD2 n2 t2 s2)  = (teamName t1 /= teamName t2) && (sum s1 == sum s2)

-- ============================================================================
-- 2. ENDTERM 2 (endterm2.hs)
-- ============================================================================

-- 2.1
applyFunction :: (Int -> Int -> Int) -> [Int] -> [Int] -> [(Int, Int, Int)]
applyFunction f lst1 lst2 = [(x, y, f x y) | x <- lst1 | y <- lst2]

-- main = print $ applyFunction (\x y -> x^2 + 2*x*y + y^2) [4,5,6] [1,2,3] 

-- 2.2
gen2 :: Int -> [(Int, Int)]
gen2 n = take n ([ (x, y) | x <- [1..], x `mod` 3 == 0 | y <- [1..], y `mod` 5 == 0 ])

-- main = print (gen2 3) -- [(3,5),(6,10),(9,15)]

-- 2.3
rec1 :: Student -> Char -> Int -> Student
rec1 std1 ch n 
    | (ch `elem` studentName std1) && (uniID (uni std1) == n)
        = std1 { uni = (uni std1) { uniID = (uniID (uni std1)) + 10 } }
    | otherwise = std1

-- main = print $ rec1 student1 'c' 1

-- 2.4
studentsAvg :: [Student] -> Double
studentsAvg lst = fromIntegral (sum gradesOnly) / fromIntegral (length gradesOnly)
    where gradesOnly = [ grds | stdnt <- lst, grds <- grades stdnt]

averageGradeForUniversity :: Univ -> [Student] -> (String, Double)
averageGradeForUniversity uni1 lst = (uniName uni1, avgGrade)
    where 
        filtered = [ std | std <- lst,  uniName (uni std) == uniName uni1]
        avgGrade = studentsAvg filtered

-- main = print $ averageGradeForUniversity univ3 [student5] 

-- 2.5
tr1 :: Tree a -> a
tr1 Leaf = error "Tree is empty"
tr1 (Node x Leaf _) =  x 
tr1 (Node _ left _) = tr1 left

-- main = print $ tr1 tree1 -- 3

-- 2.6
tr2 :: Tree a -> (a -> a) -> Tree a
tr2 Leaf _                   = Leaf 
tr2 (Node x left right) ff   = Node (ff x) (tr2 left ff) (tr2 right ff )

-- main = print $ tr2 tree2 (+100) 

-- 2.7
changeTree :: Tree Int -> Tree Int
changeTree Leaf   = Leaf
changeTree (Node n left right) 
    | leftVal + rightVal  < 0        = Node (-1) (changeTree left) (changeTree right)
    | otherwise                      = Node n (changeTree left) (changeTree right)
    where 
        leftVal  = nodeValue left
        rightVal = nodeValue right

nodeValue :: Tree Int -> Int
nodeValue Leaf          = 0
nodeValue (Node v _ _)  = v

-- main = print $ changeTree (Node 10 (Node (-5) Leaf Leaf) (Node (-6) Leaf Leaf))

-- 2.8
instance Eq (Tree Int) where 
    t1 == t2     = listed t1 == listed t2

listed :: Tree Int -> [Int]
listed Leaf = []
listed (Node x left right) = listed left ++ [x] ++ listed right

tree00,tree11,tree22,tree33:: Tree Int
tree00 = Node 10 (Node 5 (Node 3 Leaf Leaf) (Node 7 Leaf Leaf)) (Node 15 (Node 12 (Node 11 Leaf Leaf) Leaf) (Node 20 Leaf Leaf))
tree11 = Node 10 (Node 5 (Node 3 Leaf Leaf) (Node 7 Leaf Leaf)) (Node 15 (Node 12 Leaf Leaf) (Node 20 Leaf Leaf))
tree22 = Node 10 (Node 5 (Node 3 Leaf Leaf) (Node 7 Leaf Leaf)) (Node 15 (Node 12 Leaf Leaf) (Node 20 Leaf Leaf))
tree33 = Node 10 (Node 5 (Node 3 Leaf Leaf) (Node 7 Leaf Leaf)) (Node 12 (Node 11 Leaf Leaf) (Node 15 Leaf (Node 20 Leaf Leaf)))

-- main = print $ tree00 == tree33 -- True

-- 2.9
data Sport a = Football a | Voleyball a | Basketball a 

instance Show a => Show (Sport a) where
    show (Football x) = "Football: " ++ show x 
    show (Voleyball x)  = "Voleyball: " ++ show x
    show (Basketball x) = "Basketball: " ++ show x
        
-- main = print $ Football "Played by 22" -- Football: "Played by 22"

-- ============================================================================
-- 3. ENDTERM 3 (endterm3.hs)
-- ============================================================================

-- 3.1
dotProd :: [Int] -> [Int] -> Int
dotProd lst1 lst2 = sum ([ x * y | x <- lst1 | y <- lst2])

-- main = print $ dotProd [1, 2, 3] [4, 5, 6] -- 32

-- 3.2
lightStates :: String -> String -> [String]
lightStates signal1 signal2 = map (\pair-> if pair == "11" then "Off" else "On") multiplied
    where multiplied = [ [x,y] | x <- signal1 | y <- signal2 ]

-- main = print $ lightStates "1100" "1010" -- ["On", "Off", "Off", "Off"]

-- 3.3
failingStudents :: [Student] -> [(String, [Int])]
failingStudents lst = [ (studentName s, [ g | g <- grades s, g < 60 ]) | s <- lst, [ g | g <- grades s, g < 60 ] /= [] ]

-- main = print $ failingStudents [student1, student2, student7, student4, student8, student6] 

-- 3.4
topScorersInTeam :: Team -> [Player] -> [Player]
topScorersInTeam team1 playersList = [ p | p <- filtered, totalScore p == maxScore ]
    where 
        totalScoreOnePlayer p = sum (playerScores p)
        filtered = [ p | p <- playersList, teamName team1 == teamName (playerTeam p)]
        maxScore  = maximum (0 : [ totalScore p | p <- filtered ])

-- ============================================================================
-- 4. ENDTERM 4 (endterm4.hs)
-- ============================================================================

-- 4.1
gen1 :: Int -> [(Char, Char)]
gen1 n = take n [(x, y) | x <- ['A'..] | y <- ['a'..]]

-- main = print (gen1 3) -- [('A','a'),('B','b'),('C','c')]

-- 4.2
shiftPoints :: [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
shiftPoints lst1 lst2 = [ ((fst x + fst y), (snd x + snd y)) | x <- lst1 | y <- lst2 ]

-- main = print $ shiftPoints [(1, 2), (3, 4), (5, 6)] [(1, 1), (2, 2), (3, 3)] 
-- Expected output: [(2, 3), (5, 6), (8, 9)]

-- 4.3
topTeam :: [Player] -> Int
topTeam [] = error "No Team!"
topTeam lst = snd (head [ pair | pair <- teamSums, fst pair == best ])
  where
    teamSums = [ (sum [sum (playerScores p) | p <- lst, teamID (playerTeam p) == t], t)
               | t <- [ teamID (playerTeam p) | p <- lst ] ]
    best = maximum [ fst pair | pair <- teamSums ]

-- main = print $ topTeam [player1, player2, player3, player4, player5, player6] -- 3

-- 4.4
tree :: Tree Player
tree = Node player1 (Node player2 Leaf (Node player3 Leaf Leaf)) (Node player4 (Node player5 Leaf Leaf) (Node player6 Leaf Leaf))

playersFromTeam :: Team -> Tree Player -> [(Int, String)]
playersFromTeam _ Leaf = []
playersFromTeam team (Node x left right) 
    | teamName (playerTeam x) == teamName team     = (playerID x, playerName x) : (playersFromTeam team left) ++ (playersFromTeam team right)
    | otherwise                                    = (playersFromTeam team left) ++ (playersFromTeam team right)

-- main = print (playersFromTeam team1 tree) -- [(1,"Alice"),(2,"Bob")]

-- 4.5
trim :: Tree Integer -> Tree Integer
trim tree = trimHelper tree 1
  where
    trimHelper :: Tree Integer -> Int -> Tree Integer
    trimHelper Leaf _ = Leaf 
    trimHelper (Node x left right) depth 
        | depth >= 3 = Node x Leaf Leaf
        | otherwise  = Node x (trimHelper left (depth + 1)) (trimHelper right (depth + 1))

-- main = print $ trim (Node 1 (Node 2 (Node 4 Leaf Leaf) (Node 5 (Node 7 Leaf Leaf) Leaf))(Node 3(Node 6(Node 8(Node 9 Leaf Leaf) Leaf) Leaf) Leaf))

-- 4.6
isPaliTree :: Tree Char -> Bool
isPaliTree treee = (toList treee) == reverse(toList treee)

toList :: Tree Char -> [Char]
toList Leaf = []
toList (Node x left right) = (toList left) ++ [x] ++ (toList right)

-- main = print $ isPaliTree (Node 'd' (Node 'm' Leaf (Node 'a' Leaf Leaf)) (Node 'a' Leaf (Node 'm' Leaf Leaf))) -- True

-- 4.7
data BTree a = BNode (BTree a) a (BTree a) | BLeaf a 

instance Show a => Show (BTree a) where 
    show (BLeaf x) = "Leaf: " ++ show x
    show (BNode x left right) = "Node: (" ++ show left ++ ") -> " ++ show x ++ " -> (" ++ show right ++ ")"

t1, t2 :: BTree Int
t1 = BNode (BNode (BLeaf 1) 2 (BLeaf 3)) 4 (BLeaf 5)
t2 = BNode (BNode (BLeaf 6) 20 (BLeaf 5)) 10 (BNode (BLeaf 1) 8 (BLeaf 2))

-- main = print t1 -- Node: (Node: (Leaf: 1) -> 2 -> (Leaf: 3)) -> 4 -> (Leaf: 5)

-- 4.8
instance Show Player where 
    show (Player _ playerName playerTeam playerScores) = "I am " ++ playerName ++ " from " ++ teamName playerTeam ++ ", my average score is " ++ show avg ++ " which is " ++ rating ++ "."
        where 
            avg :: Double
            avg = fromIntegral (sum playerScores) / fromIntegral (length playerScores)
            rating :: String
            rating = if avg < 20 then "bad" else "quite good"

-- main = print player1

-- 4.9
instance Eq Team where 
  (Team n1 id1) == (Team n2 id2)   = n1 == n2 && id1 == id2

instance Eq Player where 
  (Player id11 n11 t11 s11) == (Player id22 n22 t22 s22) = (sum s11)==(sum s22) &&  t11 == t22

-- main = print $ player1 == player2    -- False

-- ============================================================================
-- 5. ENDTERM 5 (endterm5.hs)
-- ============================================================================

-- 5.1
avg :: [Int] -> Float
avg lst = fromIntegral (sum lst) / fromIntegral (length lst)

sortTupleList :: [(String, String, Float)] -> [(String, String, Float)]
sortTupleList lst = sortBy cmp lst
  where
    cmp (_,_,a) (_,_,b) = compare b a

topNStudents :: Int -> [Student] -> [(String, String, Float)]
topNStudents n lst = take n ( sortTupleList [(uniName (uni s), studentName s, avg (grades s)) | s <- lst] )

-- main = print $ topNStudents 5 [student1, student2, student3, student4, student5, student6] 

-- 5.2
data Mark = Excellent | Good | Satisfactory | Pass | Fail 

instance Eq Mark where 
    Excellent == Excellent   = True
    Good == Good             = True
    Satisfactory == Satisfactory   = True
    Pass == Pass   = True
    Fail == Fail   = True
    _ == _         = False

instance Show Mark where
    show Excellent    = "Excellent(5)"
    show Good         = "Good(4)"
    show Satisfactory = "Satisfactory(3)"
    show Pass         = "Pass(2)"
    show Fail         = "Fail(1)"

-- main = print $ Excellent == Excellent -- True
-- main = print $ show Excellent -- "Excellent(5)"

-- 5.3
average1 :: [Int] -> Double 
average1 lst = fromIntegral (sum lst) / fromIntegral (length lst)

failure :: [Int] -> Bool 
failure [] = False
failure (x : xs) = if x<50 then True else (failure xs)

finalMark :: Student -> Mark
finalMark std  
    | failure (grades std)                                          = Fail
    | average1 (grades std) >= 50 && average1 (grades std) < 60     = Excellent

-- main = print $ finalMark (Student {neptunID = 101, studentName = "Alice", uni = univ1, grades = [85, 90, 78]}) -- Excellent(5)

-- 5.4
instance Eq Student where 
    (Student id1 n1 u1 g1) == (Student id2 n2 u2 g2)  = ((length n1) == (length n2) && ((id1 `mod` 2 == 1) && (id2 `mod` 2 == 0))) || ((uniName u1) == (uniName u2))

-- main = print (student4 == student6)  --False

-- 5.5
listToTriple :: [[Int]] -> [(Int, Int, Int)]
listToTriple lst = [ (ind, length sub, sum sub) | (sub, ind) <- zip lst [0..] ]

-- main = print $ listToTriple [[10], [4, 5], [6, 0, 8, 9]] -- [(0,1,10),(1,2,9),(2,4,23)]

-- 5.6
compareInts :: [Int] -> [Int] -> [Int]
compareInts lst1 lst2 = [ if (abs n1)>(abs n2) then n1 else n2 | n1 <- lst1 | n2 <- lst2]

-- main = print(compareInts [100,12,13,1567] [17,-133,1087,33]) -- [100,-133,1087,1567]

-- 5.7
sortAndSelectNodesGreaterThan :: Int -> Tree Int -> [Int]
sortAndSelectNodesGreaterThan n tree = filter (> n) $ reverse $ sort $ treeToList tree
  where 
    treeToList Leaf = []
    treeToList (Node x left right) = x : treeToList left ++ treeToList right

-- main = print $ sortAndSelectNodesGreaterThan 9 tree1 -- [18, 15, 12, 10]

-- 5.8
weightedSumHelper :: Tree Int -> Int -> Int 
weightedSumHelper Leaf _  = 0
weightedSumHelper (Node x left right) depth = x*depth + (weightedSumHelper left (depth+1)) + (weightedSumHelper right (depth+1)) 

treeWeightedSum :: Tree Int -> Int
treeWeightedSum tree = weightedSumHelper tree 1 

-- main = print (treeWeightedSum tree1) -- 38

-- 5.9
leafCount :: Tree Int -> Int
leafCount Leaf  = 0
leafCount (Node _ Leaf Leaf) = 1
leafCount (Node x left right) = leafCount left + leafCount right

-- main = print (leafCount tree1) -- 3

-- 5.10
data Direction = North | South | East | West

instance Show Direction where 
    show North = "Going North"
    show South = "Going South"
    show East = "Going East"
    show West = "Going West"

instance Eq Direction where 
    North == North   = True
    South == South   = True 
    West == West     = True 
    East == East     = True 
    _ == _           = False 

-- main = print $ North -- Going North

-- ============================================================================
-- 6. TESTS (tests.hs)
-- ============================================================================

-- 6.1 (Previously test 3)
rec1_test :: Student -> Char -> Int -> Student
rec1_test std ch n 
    | (ch `elem` (studentName std)) && (uniID (uni std) == n)   = std{neptunID = (neptunID std) + 100}
    | otherwise = std

-- main = print $ rec1_test student1 'c' 1

-- 6.2
tr1_test :: Tree a -> a
tr1_test Leaf = error "Tree is empty"
tr1_test (Node x Leaf _) =  x 
tr1_test (Node _ left _) = tr1_test left

-- main = print $ tr1_test tree1 -- 3

-- 6.3
tr2_test :: Tree a -> (a -> a) -> Tree a
tr2_test Leaf _                   = Leaf 
tr2_test (Node x left right) ff   = Node (ff x) (tr2_test left ff) (tr2_test right ff )

-- main = print $ tr2_test tree2 (+100) 

-- 6.4
changeTree_test :: Tree Int -> Tree Int
changeTree_test Leaf   = Leaf
changeTree_test (Node n left right) 
    | leftVal + rightVal  < 0        = Node (-1) (changeTree_test left) (changeTree_test right)
    | otherwise                      = Node n (changeTree_test left) (changeTree_test right)
    where 
        leftVal  = nodeValue_test left
        rightVal = nodeValue_test right

nodeValue_test :: Tree Int -> Int
nodeValue_test Leaf          = 0
nodeValue_test (Node v _ _)  = v

-- main = print $ changeTree_test (Node 10 (Node (-5) Leaf Leaf) (Node (-6) Leaf Leaf))

-- 6.5
instance Eq (Tree Int) where 
    t1 == t2     = listed t1 == listed t2

listed_test :: Tree Int -> [Int]
listed_test Leaf = []
listed_test (Node x left right) = listed_test left ++ [x] ++ listed_test right

tree00_test,tree11_test,tree22_test,tree33_test:: Tree Int
tree00_test = Node 10 (Node 5 (Node 3 Leaf Leaf) (Node 7 Leaf Leaf)) (Node 15 (Node 12 (Node 11 Leaf Leaf) Leaf) (Node 20 Leaf Leaf))
tree11_test = Node 10 (Node 5 (Node 3 Leaf Leaf) (Node 7 Leaf Leaf)) (Node 15 (Node 12 Leaf Leaf) (Node 20 Leaf Leaf))
tree22_test = Node 10 (Node 5 (Node 3 Leaf Leaf) (Node 7 Leaf Leaf)) (Node 15 (Node 12 Leaf Leaf) (Node 20 Leaf Leaf))
tree33_test = Node 10 (Node 5 (Node 3 Leaf Leaf) (Node 7 Leaf Leaf)) (Node 12 (Node 11 Leaf Leaf) (Node 15 Leaf (Node 20 Leaf Leaf)))

-- main = print $ tree11_test == tree22_test -- True

-- 6.6
data Sport_test a = Football_test a | Voleyball_test a | Basketball_test a 

instance Show a => Show (Sport_test a) where
    show (Football_test x) = "Football: " ++ show x 
    show (Voleyball_test x)  = "Voleyball: " ++ show x
    show (Basketball_test x) = "Basketball: " ++ show x
        
-- main = print $ Football_test "Played by 22" -- Football: "Played by 22"