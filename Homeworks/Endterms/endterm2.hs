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


-- ************************************************

--1
applyFunction :: (Int -> Int -> Int) -> [Int] -> [Int] -> [(Int, Int, Int)]
applyFunction f lst1 lst2 = [(x, y, f x y) | x <- lst1 | y <- lst2]

-- main = print $ applyFunction (\x y -> x^2 + 2*x*y + y^2) [4,5,6] [1,2,3] 

--2
gen2 :: Int -> [(Int, Int)]
gen2 n = take n ([ (x, y) | x <- [1..], x `mod` 3 == 0 | y <- [1..], y `mod` 5 == 0 ])

-- main = print (gen2 3) -- [(3,5),(6,10),(9,15)]

--3
rec1 :: Student -> Char -> Int -> Student
rec1 std1 ch n 
    | (ch `elem` studentName std1) && (uniID (uni std1) == n)
        = std1 { uni = (uni std1) { uniID = (uniID (uni std1)) + 10 } }
    | otherwise = std1

-- main = print $ rec1 student1 'c' 1

--4
studentsAvg :: [Student] -> Double
studentsAvg lst = fromIntegral (sum gradesOnly) / fromIntegral (length gradesOnly)
    where gradesOnly = [ grds | stdnt <- lst, grds <- grades stdnt]

-- main = print $ studentsAvg [student1] -- 84.33333333333333

averageGradeForUniversity :: Univ -> [Student] -> (String, Double)
averageGradeForUniversity uni1 lst = (uniName uni1, avgGrade)
    where 
        filtered = [ std | std <- lst,  uniName (uni std) == uniName uni1]
        avgGrade = studentsAvg filtered

-- main = print $ averageGradeForUniversity univ3 [student5] 


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

--5
tr1 :: Tree a -> a
tr1 Leaf = error "Tree is empty"
tr1 (Node x Leaf _) =  x 
tr1 (Node _ left _) = tr1 left

-- main = print $ tr1 tree1 -- 3

--6
tr2 :: Tree a -> (a -> a) -> Tree a
tr2 Leaf _                   = Leaf 
tr2 (Node x left right) ff   = Node (ff x) (tr2 left ff) (tr2 right ff )

-- main = print $ tr2 tree2 (+100) 
-- Node 100 (Node 101 (Node 105 Leaf (Node 108 Leaf Leaf)) Leaf) (Node 102 Leaf Leaf)

--7
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

--8

--this compared the structure too, if we just want to compare by values, this wont work
-- instance Eq (Tree Int) where
--     Leaf == Leaf    = True
--     (Node x left right) == (Node y left1 right1)   = x==y && left==left1 && right==right1
--     _ == _             = False

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

--9
data Sport a = Football a | Voleyball a | Basketball a 

instance Show a => Show (Sport a) where
    show (Football x) = "Football: " ++ show x 
    show (Voleyball x)  = "Voleyball: " ++ show x
    show (Basketball x) = "Basketball: " ++ show x
        
-- main = print $ Football "Played by 22" -- Football: "Played by 22"
-- main = print $ Basketball 5 -- Basketball: 5
-- main = print $ Voleyball True -- Voleyball: True
-- main = print $ Basketball [1,2,3,4] -- Basketball: [1,2,3,4]





