-- WRITE NAME AND NEPTUN CODE HERE !!!!

{-# LANGUAGE ParallelListComp #-}
import Data.List

main :: IO ()

data Univ = Univ {uniName :: String, uniID :: Int} deriving (Show)

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

------------------------------------------------------------------------------

{-1.
Calculate the average scores for each student and create a list of the results.
From that list, extract the top N students based on their average scores and
present them as tuples in the format (university name, student name, average score),
with the avg score in descending order.
In case less than N students write "Not enough students!"
 -}

avg :: [Int] -> Float
avg lst = fromIntegral (sum lst) / fromIntegral (length lst)

sortTupleList :: [(String, String, Float)] -> [(String, String, Float)]
sortTupleList lst = sortBy cmp lst
  where
    cmp (_,_,a) (_,_,b) = compare b a

topNStudents :: Int -> [Student] -> [(String, String, Float)]
topNStudents n lst = take n ( sortTupleList [(uniName (uni s), studentName s, avg (grades s)) | s <- lst] )

-- main = print $ topNStudents 5 [student1, student2, student3, student4, student5, student6] 
-- [("University B","David",89.333336),("University A","Bob",86.666664),("University C","Frank",86.0),
--  ("University A","Alice",84.333336),("University C","Eve",83.666664)]
--main = print $ topNStudents 2 [student1, student2, student6] 
-- [("University A","Bob",86.666664),("University C","Frank",86.0)]
--main = print $ topNStudents 3 [student1, student2] -- Not enough students!
--main = print $ topNStudents 5 [] -- Not enough students!

------------------------------------------------------------------------------

{-2.
Define an enumeration type `Mark` with the values `Excellent`, `Good`, `Satisfactory`, `Pass`, `Fail`.
a) Implement the Eq instance for the `Mark` type. Values are equal if they represent the same state.
   Deriving Eq will not be accepted.
b) Create a custom show instance that converts each value as follows:
   Excellent -> "Excellent(5)", Good -> "Good(4)", Satisfactory -> "Satisfactory(3)", Pass -> "Pass(2)", Fail -> "Fail(1)"
-}

-- Mark type comes here
data Mark = Excellent | Good | Satisfactory | Pass | Fail 

--instancing eqality 
instance Eq Mark where 
    Excellent == Excellent   = True
    Good == Good             = True
    Satisfactory == Satisfactory   = True
    Pass == Pass   = True
    Fail == Fail   = True
    _ == _         = False


--instancing showing
instance Show Mark where
    show Excellent    = "Excellent(5)"
    show Good         = "Good(4)"
    show Satisfactory = "Satisfactory(3)"
    show Pass         = "Pass(2)"
    show Fail         = "Fail(1)"


--main = print $ Excellent == Excellent -- True
--main = print $ Excellent == Good -- False
--main = print (show Excellent ++ " " ++ show Good ++ " " ++ show Satisfactory ++ " " ++ show Pass ++ " " ++ show Fail) 
-- Excellent(5) Good(4) Satisfactory(3) Pass(2) Fail(1)

------------------------------------------------------------------------------

{-3.
Write a function that takes a student and returns the Final mark of the student based on the average of the grades.
If the student has at least one grade less than 50, the final mark is Fail regardless of the average.
For the student who has greater than or equal to 50 on every grade the final mark is calculated as follows:
       average < 50 -> Fail
 50 <= average < 60 -> Pass
 60 <= average < 70 -> Satisfactory
 70 <= average < 80 -> Good
 80 <= average      -> Excellent
-}

average1 :: [Int] -> Double 
average1 lst = fromIntegral (sum lst) / fromIntegral (length lst)

failure :: [Int] -> Bool 
failure [] = False
failure (x : xs) = if x<50 then True else (failure xs)

finalMark :: Student -> Mark
finalMark std  
    | failure (grades std)                                          = Fail
    | average1 (grades std) >= 50 && average1 (grades std) < 60     = Excellent


--main = print $ finalMark (Student {neptunID = 101, studentName = "Alice", uni = univ1, grades = [85, 90, 78]}) -- Excellent(5)
--main = print $ finalMark (Student {neptunID = 102, studentName = "Bob", uni = univ1, grades = [51, 62, 53]}) -- Pass(2)
--main = print $ finalMark (Student {neptunID = 103, studentName = "Charlie", uni = univ2, grades = [75, 85, 42]}) -- Fail(1)
--main = print $ finalMark (Student {neptunID = 104, studentName = "David", uni = univ2, grades = [82, 69, 72]}) -- Good(4)

------------------------------------------------------------------------------

{-4.
Implement equality for the students record. Two students are equal if:
the students go to the same university OR 
their names are of the same length, in which case one of the student's Neptune code must be odd number.
Example:
Student {neptunID = 104, studentName = "David", uni = univ2, grades = [90, 87, 91]} == 
Student {neptunID = 106, studentName = "Frank", uni = univ3, grades = [85, 89, 84]}
    1. The students do not go to the same university
    2. But their name is the same length but none of their neptune codes are odd numbers 
    so these students are NOT equal!
-}

--instancing equality
instance Eq Student where 
    (Student id1 n1 u1 g1) == (Student id2 n2 u2 g2)  = ((length n1) == (length n2) && ((id1 `mod` 2 == 1) && (id2 `mod` 2 == 0))) || ((uniName u1) == (uniName u2))

-- main = print (student4 == student6)  --False
-- main = print (student1 == student2)  --True
-- main = print (student5 == student2)  --True

------------------------------------------------------------------------------

{-5.
Write a function that takes a list of lists of integers and returns a list of triple tuples.
Each tuple contains the index of the sublist (index starts from 0),
the length of the sublist, and the sum of the sublist. 
Parallel solution 10p, non-parallel solution 8p.
-}

listToTriple :: [[Int]] -> [(Int, Int, Int)]
listToTriple lst = [ (ind, length sub, sum sub) | (sub, ind) <- zip lst [0..] ]

-- main = print $ listToTriple [[10], [4, 5], [6, 0, 8, 9]] -- [(0,1,10),(1,2,9),(2,4,23)]
--main = print $ listToTriple [[1, 2, 3], [4, 5], [6, 7, 8, 9], [10, 11, 12], [13, 14, 15]] 
   -- [(0,3,6),(1,2,9),(2,4,30),(3,3,33),(4,3,42)]
--main = print $ listToTriple [] -- []

------------------------------------------------------------------------------

{-6.
Write a function that takes two integer lists and makes a new list such that
integers at same position in both lists are compared and the integer with 
more digits is kept.
Parallel solution 10p, non-parallel solution 8p.
-}

compareInts :: [Int] -> [Int] -> [Int]
compareInts lst1 lst2 = [ if (abs n1)>(abs n2) then n1 else n2 | n1 <- lst1 | n2 <- lst2]

-- eachDigit :: Int -> [Int]
-- eachDigit n = eachDigit' (abs n)
--   where
--     eachDigit' 0 = []
--     eachDigit' x = eachDigit' (x `div` 10) ++ [x `mod` 10]

-- main = print(compareInts [100,12,13,1567] [17,-133,1087,33]) -- [100,-133,1087,1567]
-- main = print (compareInts [-10,-100,-1000] [1,10,999]) -- [-10,-100,-1000]

------------------------------------------------------------------------------

{-7.
Write a function sortAndSelectNodesGreaterThan that sorts all the nodes of a 
binary tree in descending order and then selects the nodes that are greater than a given value n.
Hint: you can use "sort" from import Data.List to sort in increasing order.
-}

data Tree a = Node a (Tree a) (Tree a) | Leaf

-- Example trees
treeExample7 :: Tree Int
treeExample7 = Node 10 
                    (Node 5 
                        (Node 3 Leaf Leaf) 
                        (Node 7 Leaf Leaf)
                    )
                    (Node 15 
                        (Node 12 Leaf Leaf) 
                        (Node 18 Leaf Leaf)
                    )

treeExample8 :: Tree Int
treeExample8 = Node 6 
                    (Node 4 
                        (Node 1 Leaf Leaf) 
                        (Node 2 Leaf Leaf)
                    )
                    (Node 8 
                        (Node 9 Leaf Leaf) 
                        (Node 7 Leaf Leaf)
                    )

treeToList :: Tree a -> [a]
treeToList Leaf = []
treeToList (Node x left right) = x : treeToList left ++ treeToList right

sortAndSelectNodesGreaterThan :: Int -> Tree Int -> [Int]
sortAndSelectNodesGreaterThan n tree =
    filter (> n) $ reverse $ sort $ treeToList tree

--main = print $ sortAndSelectNodesGreaterThan 9 treeExample7
-- Expected Output: [18, 15, 12, 10]
--main = print $ sortAndSelectNodesGreaterThan 5 treeExample8
-- Expected Output: [9, 8, 7, 6]

------------------------------------------------------------------------------

{-8.
Implement a function that performs a generalized depth-based calculation for any tree. 
The function takes a tree and returns depth-weighted sum. In a depth-weighted sum,
each node value is multiplied by its depth, and the results are summed.
    For the tree:
            1    depth 1
           / \
          2   3  depth 2
         / \
        4   5    depth 3
    The depth-weighted sum should be:
    1*1 + 2*2 + 3*2 + 4*3 + 5*3 = 38
-}

weightedSumHelper :: Tree Int -> Int -> Int 
weightedSumHelper Leaf _  = 0
weightedSumHelper (Node x left right) depth = x*depth + (weightedSumHelper left (depth+1)) + (weightedSumHelper right (depth+1)) 

treeWeightedSum :: Tree Int -> Int
treeWeightedSum tree = weightedSumHelper tree 1 

-- treeWeightedSum :: Tree Int -> Int
-- treeWeightedSum tree = weightedSumHelper tree 1  -- start at depth 1
--   where
--     weightedSumHelper :: Tree Int -> Int -> Int
--     weightedSumHelper Leaf _ = 0
--     weightedSumHelper (Node x left right) depth =
--         x * depth + weightedSumHelper left (depth + 1) + weightedSumHelper right (depth + 1)


tree11 = Node 10 Leaf Leaf
tree22 = Node 1 (Node 2 Leaf Leaf) (Node 3 Leaf Leaf)
tree33 = Node 5 (Node 4 (Node 3 Leaf Leaf) Leaf) Leaf
tree44 = Node 1 (Node 2 (Node 4 Leaf Leaf) (Node 5 Leaf Leaf)) (Node 3 Leaf Leaf)

-- main = print (treeWeightedSum tree11) -- 10
--main = print (treeWeightedSum tree22) -- 11
--main = print (treeWeightedSum tree33) -- 22
--main = print (treeWeightedSum tree44) -- 38

------------------------------------------------------------------------------

{-9.
Write a function that takes a tree and returns the number of terminal nodes in the tree.
A terminal node is a node with leaves children.
        1
       /  \
      2     3
     /\    / \
    L  L  4   5
         /\  /\
        L L  L L  terminal nodes are 2, 4, and 5. Result: 3
-}

tree1 = Node 1 (Node 2 Leaf Leaf) (Node 3 (Node 4 Leaf Leaf) (Node 5 Leaf Leaf))
tree2 = Node 1 Leaf Leaf
tree3 = Leaf
tree4 = Node 1 (Node 2 (Node 6 Leaf Leaf) Leaf) (Node 3 Leaf (Node 4 Leaf Leaf))

leafCount :: Tree Int -> Int
leafCount Leaf  = 0
leafCount (Node _ Leaf Leaf) = 1
leafCount (Node x left right) = leafCount left + leafCount right

-- main = print([leafCount tree1,leafCount tree4,leafCount tree3,leafCount tree2]) -- [3,2,0,1]

------------------------------------------------------------------------------

{-10.
Implement the Show and Eq methods for the Direction.
Show should return in following way: eg. for North output is "Going North"
-}

data Direction = North | South | East | West

--instancing showing
instance Show Direction where 
    show North = "Going North"
    show South = "Going South"
    show East = "Going East"
    show West = "Going West"

--instancing eqality 
instance Eq Direction where 
    North == North   = True
    South == South   = True 
    West == West     = True 
    East == East     = True 
    _ == _           = False 

--main = print $ North -- Going North
--main = print $ East -- Going East
--main = print $ West -- Going West

--main = print $ East == East -- True
--main = print $ South == West  -- False
--main = print $ North == East  -- False

------------------------------------------------------------------------------
