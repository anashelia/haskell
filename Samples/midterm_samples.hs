{-# LANGUAGE ParallelListComp #-}
import Data.Char
import Data.List

main :: IO ()
-- main = print "Run individual tests by uncommenting them below."

-------------------------------------------------------------------------------
-- 1. MIDTERM SAMPLE D (midtermD.hs)
-------------------------------------------------------------------------------

-- 1.1 Create a function that takes an integer and counts 
-- the sum of even digits less than 5, for negative 
-- numbers consider the absolute value of it.

eu5helper :: Int -> Int
eu5helper n
  | n<5 && even(n) = n
  | otherwise = 0

evenUnder5 :: Int -> Int
evenUnder5 n
  | n < 0 = evenUnder5(n*(-1))
  | n < 10 = eu5helper(n)
  | otherwise = eu5helper(n `rem` 10) + evenUnder5(n `div` 10)
  
--main = print(evenUnder5(12540)) -- 6
--main = print(evenUnder5(-12540)) -- 6
--main = print(evenUnder5(7624335642)) -- 12
--main = print(evenUnder5(0)) -- 0
--main = print(evenUnder5(7)) -- 0

-- 1.2 Given an n, and a list of words. Write a function that 
-- keeps any word with n or more characters. 

filterWords :: Int -> [String] -> [String]
filterWords n words = filter (\word -> length word >= n) words

--main = print (filterWords 5 ["hello", "to", "the", "world", "of", "Haskell"]) -- ["hello","world","Haskell"]
--main = print (filterWords 3 ["Functional", "Programming", "is", "very", "cool"]) -- ["Functional","Programming","very","cool"]
--main = print (filterWords 6 ["I", "love", "AI"]) -- []
--main = print (filterWords 2 []) -- []

-- 1.3 Write a function that takes two lists of strings and returns 
-- a list of all pairs (x, y) such that y is the inverted x,
-- where x is from the first list and y is from the second one.

mirrorPairs :: [String] -> [String] -> [(String, String)]
mirrorPairs xs ys = [(x, y) | x <- xs, y <- ys, reverse x == y]

--main = print (mirrorPairs ["abc", "123", "live", "a"] ["cba", "321", "evil", "b", "123"]) -- [("abc","cba"),("123","321"),("live","evil")]
--main = print (mirrorPairs ["hello", "world"] ["olleh", "dlrow"]) -- [("hello","olleh"),("world","dlrow")]
--main = print (mirrorPairs ["foo", "bar"] ["bax", "rab", "qux", "rab", "rab", ""]) -- [("bar","rab"),("bar","rab"),("bar","rab")]
--main = print (mirrorPairs ["", "bar"] [""]) -- [("","")]

-- 1.4 Create a function that takes a list of list of integers 
-- and keeps only those lists that have the first item as an even 
-- number. If the sublist is empty, do not put it in the result.

fstEvenHelp :: [Int] -> Bool
fstEvenHelp [] = False
fstEvenHelp (x:xs)
  | even(x) = True
  | otherwise = False
  
fstEven :: [[Int]] -> [[Int]]
fstEven list = filter fstEvenHelp list

--main = print(fstEven([[1,3,2],[2,5,6],[4,5],[68,1],[2],[1..10],[11..50]])) -- [[2,5,6],[4,5],[68,1],[2]]
--main = print(fstEven([[1,2,2],[1,2,5,6],[1],[68,1],[5,7],[3,4,5],[2]])) -- [[68,1],[2]]

-- 1.5 Write a function that takes a tuple of 2 non-empty lists and 
-- creates a new Integer list made by joining first item of 2nd list
-- with everything except the first item of 1st list.

swapTup :: ([Int],[Int]) -> [Int]
swapTup (a,b) = [head b] ++ tail a

--main = print(swapTup ([1,2,3,4],[5,6,7,8])) -- [5,2,3,4]
--main = print(swapTup ([1..10],[100,99..90])) -- [100,2,3,4,5,6,7,8,9,10]

-- 1.6 Given a list of (a,b) pairs, return the list of a+b for each tuple.

sumPairs :: [(Int,Int)] -> [Int]
sumPairs ps = map (\(a,b) -> a + b) ps

--main = print(sumPairs [(1,2),(3,4)])      -- [3,7]
--main = print(sumPairs [])                  -- []
--main = print(sumPairs [(10,-3),(5,5),(1,1),(-1,1),(3,3)]) -- [7,10,2,0,6]

-- 1.7 Given a function f and a list, apply f to each consecutive x,y,z of the list
-- and return the result. If fewer than 3 elements, return [].

applyTriples :: (a -> a -> a -> b) -> [a] -> [b]
applyTriples _ [] = []
applyTriples _ [_] = []
applyTriples _ [_,_] = []
applyTriples f (x:y:z:xs) = f x y z : applyTriples f (y:z:xs)

sum3 :: Int -> Int -> Int -> Int
sum3 a b c = a + b + c

prod3 :: Int -> Int -> Int -> Int
prod3 a b c = a * b * c

max3 :: Int -> Int -> Int -> Int
max3 a b c = max a (max b c)

--main = print (applyTriples sum3 [1,2,3,4,5])   -- [6,9,12]
--main = print (applyTriples sum3 [1])           -- []
--main = print (applyTriples sum3 [5,10,15])     -- [30]
--main = print (applyTriples prod3 [2,3,4,5])    -- [24,60]

-- 1.8 Given a predicate p and a list of integers,
-- return the sum of the squares of the elements that satisfy p.

sumSquaresIf :: (Int -> Bool) -> [Int] -> Int
sumSquaresIf p xs = sum (map (\x -> x * x) (filter p xs))

--main = print(sumSquaresIf (\x -> even x) [1,2,3,4,5]) -- 20
--main = print(sumSquaresIf (\x -> x > 10) [5,10,11,3,12]) -- 265
--main = print(sumSquaresIf even [1]) -- 0

-- 1.9 Write a function that takes a number and returns all pairs 
-- of its digits. A digit is not paired up with itself.

digitPairs :: Int -> [(Int, Int)]
digitPairs n = [(d1, d2) | d1 <- digits n, d2 <- digits n, d1 /= d2]
  where
    digits 0 = [0]
    digits x = reverse (digitsHelper x)
    digitsHelper 0 = []
    digitsHelper y = (y `mod` 10) : digitsHelper (y `div` 10)

--main = print (digitPairs 123)     -- [(1,2),(1,3),(2,1),(2,3),(3,1),(3,2)]
--main = print (digitPairs 45)      -- [(4,5),(5,4)]
--main = print (digitPairs 77)      -- []
--main = print (digitPairs 0)       -- []

-- 1.10 Define a function to count how many words are in a dictionary.
-- The function takes two arguments:
--   1. a list of words to check: [String] (assume there are no duplicates)
--   2. a dictionary list: [(String, Int)], where each tuple contains
--      a word and the page number where that word appears.
-- The function returns how many of the given words appear in the dictionary, 
-- regardless of their page number.

countWords :: [String] -> [(String, Int)] -> Int
countWords words dict = length [w | w <- words, w `elem` d]
  where d = map fst dict

--main = print (countWords ["apple","pear","banana"] [("apple",1),("banana",3),("cherry",4)])  -- 2
--main = print (countWords ["dog","cat","bird","cat","cat"]  [("cat",2),("dog",5),("fish",7),("fox",15)]) -- 4
--main = print (countWords ["tree","leaf","branch"] [("root",1),("stem",2)]) -- 0


-------------------------------------------------------------------------------
-- 2. ENDTERM 90 SOLVED (endterm90_solved.hs)
-------------------------------------------------------------------------------

data Univ = Univ { uniName :: String, uniID :: Int }
  deriving (Show, Eq)

data Student = Student
  { neptunID    :: Int
  , studentName :: String
  , uni         :: Univ
  , grades      :: [Int]
  } deriving (Show)

univ1 = Univ {uniName = "University A", uniID = 1}
univ2 = Univ {uniName = "University B", uniID = 2}
univ3 = Univ {uniName = "University C", uniID = 3}

student1 = Student {neptunID = 101, studentName = "Alice",   uni = univ1, grades = [85, 90, 78]}
student2 = Student {neptunID = 102, studentName = "Bob",     uni = univ1, grades = [88, 92, 80]}
student3 = Student {neptunID = 103, studentName = "Charlie", uni = univ2, grades = [75, 85, 82]}
student4 = Student {neptunID = 104, studentName = "David",   uni = univ2, grades = [90, 87, 91]}
student5 = Student {neptunID = 105, studentName = "Eve",     uni = univ3, grades = [80, 83, 88]}
student6 = Student {neptunID = 106, studentName = "Frank",   uni = univ3, grades = [85, 89, 84]}
student7 = Student {neptunID = 100, studentName = "Brian Hood", uni = univ3, grades = [5,9,4]}

studentsAll :: [Student]
studentsAll = [student1, student2, student3, student4, student5, student6]

-- 2.1 A university wants to reward students by adding a bonus to 
-- each grade. Write a function that takes a bonus amount and 
-- a Student, and returns an updated Student where:
--   - Each grade is increased by the bonus.
--   - No grade can be higher than 100.

bonusStudent :: Int -> Student -> Student
bonusStudent n st = st {grades = map (\x -> min 100 (x+n)) $ grades st}

--main = print $ bonusStudent 10 student1
-- Student {neptunID = 101, studentName = "Alice", uni = Univ {uniName = "University A", uniID = 1}, grades = [95,100,88]}

-- 2.2 Write a function that, given a university and a list of students,
-- returns the names of all students enrolled to that university.

studentsFromUniversity :: Univ -> [Student] -> [String]
studentsFromUniversity u list = [studentName x | x <- list, (uni x) == u]

--main = print $ studentsFromUniversity univ1 studentsAll -- ["Alice","Bob"]

-- 2.3 You are given a list of students and a university name.
-- Calculate the average grades of students grouped by their university.
-- The function returns a tuple containing a list of student names 
-- from the specified university and the calculated average grade.

avg :: [Student] -> Double 
avg [] = 0.0
avg list = (fromIntegral $ sum [sum $ grades x | x<-list]) / (fromIntegral $ sum [length $ grades x | x<-list])

f1_avg :: [Student] -> String -> ([String], Double)
f1_avg list str = ([studentName x | x<-list, str == (uniName (uni x))], avg [x | x<-list, str == (uniName (uni x))])

--main = print $ f1_avg studentsAll "University A" -- (["Alice","Bob"], 85.5)

-- 2.4 You are given a list of students and a score threshold.
-- Each student has a list of scores.
-- Return a list containing only those students' name 
-- whose each score is more than the given threshold.

f3 :: [Student] -> Int -> [String]
f3 list n = [studentName x | x<-list, and $ map (>=n) $ grades x]

--main = print $ f3 studentsAll 80 -- ["Bob","David","Eve","Frank"]

-- Definition of a binary tree
data Tree a = Leaf | Node a (Tree a) (Tree a)
  deriving (Show)

tree1 :: Tree Int
tree1 =
  Node 5
    (Node 3
      (Node 1 Leaf Leaf)
      (Node 4 Leaf Leaf))
    (Node 8
      Leaf
      (Node 10 Leaf Leaf))

tree2 :: Tree Int
tree2 =
  Node 7
    (Node 2
      Leaf
      (Node 3 Leaf Leaf))
    (Node 9
      (Node 1 Leaf Leaf)
      Leaf)

-- 2.5 Count the number of even branch nodes in a binary tree.
-- A branch node is a Node whose left and right children 
-- are both leaves. The empty tree (Leaf) has 0 branch nodes.

countBN :: Tree Int -> Int
countBN Leaf = 0
countBN (Node x Leaf Leaf) = 1 - (x `mod` 2)
countBN (Node _ l r) = countBN l + countBN r 

--main = print $ countBN Leaf  -- 0
--main = print $ countBN (Node 3 Leaf Leaf) -- 0
--main = print $ countBN tree1 -- 2
--main = print $ countBN tree2 -- 0

-- 2.6 Write a function that, given a level k (starting from 0 at the root)
-- and a tree of integers, computes the sum of all node values of level k.

sumAtLevel :: Int -> Tree Int -> Int
sumAtLevel _ Leaf = 0
sumAtLevel 0 (Node x _ _) = x
sumAtLevel k (Node _ l r) = sumAtLevel (k-1) l + sumAtLevel (k-1) r

--main = print $ sumAtLevel 0 tree1 -- 5
--main = print $ sumAtLevel 1 tree1 -- 11
--main = print $ sumAtLevel 2 tree1 -- 15
--main = print $ sumAtLevel 3 tree1 -- 0

-- 2.7 You are given a tree, a predicate, and a function.
-- Return a tree where each node's value is transformed 
-- by the function only if it satisfies the predicate.

f5 ::  Tree a -> (a -> Bool) -> (a -> a) -> Tree a
f5 Leaf _ _ = Leaf
f5 (Node x l r) p f 
 | p x = Node (f x) (f5 l p f) (f5 r p f)
 |otherwise = Node x (f5 l p f) (f5 r p f)

--main = print $ f5 (Node 0 (Node 1 (Node 3 Leaf Leaf) (Node 4 Leaf Leaf)) (Node 2 (Node 5 Leaf Leaf) (Node 6 Leaf Leaf))) even (*2) 

-- 2.8 Create a custom data type MyTree with 2 constructors: MyNode 
-- and MyLeaf, such as the given samples are correct. Write Show 
-- instance for the defined type that displays the tree in the 
-- following format: [left subtree]--Node value--[right subtree].

data MyTree a = MyLeaf | MyNode a (MyTree a) (MyTree a)

myTree1 = MyNode 5 (MyNode 3 MyLeaf MyLeaf) (MyNode 7 MyLeaf MyLeaf)
myTree2 = MyNode 1 (MyNode 2 (MyNode 4 MyLeaf MyLeaf) (MyNode 5 MyLeaf MyLeaf)) (MyNode 3 (MyNode 6 MyLeaf MyLeaf) (MyNode 7 MyLeaf MyLeaf))
myTree3 = MyNode 10 (MyNode 20 (MyNode 30 MyLeaf MyLeaf) MyLeaf) (MyNode 40 MyLeaf MyLeaf)

instance Show a => Show (MyTree a) where
 show ::  (MyTree a) -> String
 show MyLeaf = "L"
 show (MyNode x l r) = "[" ++ show l ++  "--" ++ show x ++ "--" ++ show r ++ "]"

--main = print (MyLeaf :: MyTree Int) -- L    
--main = print myTree1 -- [[L]--3--[L]]--5--[[L]--7--[L]]

-- 2.9 Define an enumeration type `TrafficLight` with values: 
-- Red, Yellow, Green.
-- 1. Implement `Show` instance printing for Red -> "Stop", 
-- for Yellow -> "Wait", for Green -> "Go".
-- 2. Implement `Eq` instance, all values are equal to themselves.
-- 3. Write a function `nextLight` that changes a colour like: 
-- Red -> Green -> Yellow -> Red.

data TrafficLight = Red | Yellow | Green 

instance Show TrafficLight where
    show Red = "Stop"
    show Yellow = "Wait"
    show Green = "Go"

instance Eq TrafficLight where
 Red == Red = True
 Green == Green = True
 Yellow == Yellow = True
 _ == _ = False

nextLight :: TrafficLight -> TrafficLight
nextLight c
 | c == Red = Green
 | c == Green = Yellow 
 | otherwise = Red

--main = print(Red) -- Stop
--main = print (nextLight Red) -- Go

-- 2.10 Define the 'Number' type with 3 constructors:
-- 'NInt' is the constructor for an Int Number.
-- 'NFloat' is the constructor for a Float Number.
-- 'Invalid' does not have data associated to it.
-- Write a function that adds 10 to a Number value
-- and displays it as a string. For Invalid print "Invalid input".

data Number = NInt Int | NFloat Float | Invalid

add10 :: Number -> String
add10 Invalid = "Invalid input"
add10 (NFloat x) = show (x + 10.0)
add10 (NInt x) = show (x + 10)

--main = print (add10 (NInt 20)) -- "30"
--main = print (add10 Invalid) -- "Invalid input"


-------------------------------------------------------------------------------
-- 3. MIDTERM 92 SOLVED (midterm92_solved.hs)
-------------------------------------------------------------------------------

-- 3.1 Given a non-negative integer n, compute the product of its non-zero digits.
-- If n = 0 return 0. Examples: 105 -> 1*5 = 5, 240 -> 2*4 = 8, 0 -> 0

prodNonZeroDigits :: Int -> Int
prodNonZeroDigits 0 = 0
prodNonZeroDigits n = product (map digitToInt (filter (\x -> digitToInt x /= 0) (show n)))

-- main = print (prodNonZeroDigits 105) -- 5
-- main = print (prodNonZeroDigits 240) -- 8
-- main = print (prodNonZeroDigits 0)   -- 0

-- 3.2 Count how many times a digit d (0..9) appears in a non-negative 
-- integer n. E.g.: countDigit 12231 2 -> 2

countDigit :: Int -> Int -> Int
countDigit n i = length $ filter (\x -> digitToInt x == i) (show n)

-- main = print (countDigit 12231 2)  -- 2
-- main = print (countDigit 0 0)      -- 1

-- 3.3 Write a function that checks if the first list is 
-- a prefix of the second list. [] is a prefix of any list.

isPrefix :: [Int] -> [Int] -> Bool
isPrefix pref list = pref == take (length pref) list

-- main = print $ isPrefix [1, 2] [1, 2, 3, 4] -- True
-- main = print $ isPrefix [2, 3] [1, 2, 3, 4] -- False

-- 3.4 Write a function that takes a predicate, a function, and a list. 
-- It applies the function only to elements that satisfy the predicate.

helpFilterApply :: (a -> Bool) -> (a -> a) -> a -> a
helpFilterApply pred f x
   | pred x = f x
   | otherwise = x

filterApply :: (a -> Bool) -> (a -> a) -> [a] -> [a]
filterApply  pred func list = map (helpFilterApply pred func) list

-- main = print (filterApply (>5) (*2) [1,6,3,8,4])     -- [1,12,3,16,4]
-- main = print (filterApply even (+10) [1,2,3,4,5])    -- [1,12,3,14,5]

-- 3.5 Make triples (i,a_i,b_i) from two lists, pair elements of same 
-- position and annotate by the index. Truncate to the shorter list.

indexedTriples :: [a] -> [b] -> [(Int,a,b)]
indexedTriples list1 list2 = [(i,x,y) | i <- [0..] | x <- list1 | y <- list2]

-- main = print (indexedTriples ["x","y"] [10,20])   -- [(0,"x",10),(1,"y",20)]
-- main = print (indexedTriples [1,2,3] ['a','b'])   -- [(0,1,'a'),(1,2,'b')]

-- 3.6 Write a function that takes two lists of strings and joins 
-- the strings of same positions together and creates a new 
-- string, assume both lists always have same number of strings.

stJoin :: [String] -> [String] -> String
stJoin list1 list2 = concat [a ++ b | (a,b) <- zip list1 list2]

-- main = print (stJoin ["John "," a "," man"] ["is","good"," !"]) -- "John is a good man !"
-- main = print (stJoin ["Peter ","picked ","pack ", "pickled "] ["Piper ","a ","of ","peppers."]) -- "Peter Piper picked a pack of pickled peppers."

-- 3.7 Given list of (name, score), return the name with highest score.
-- If the list is empty, return the error "Empty list!".

bestStudent :: [(String,Int)] -> String
bestStudent [] = error "Empty list!"
bestStudent list = [name | (name, s) <- list, s == maxScore] !! 0
      where 
            maxScore = maximum $ snd $ unzip list

-- main = print(bestStudent [("Ana",90),("Bob",85)])       -- "Ana"
-- main = print(bestStudent [("Chris",70)])                -- "Chris"

-- 3.8 Write a function that counts how many elements in a list 
-- satisfy a given predicate p.

countBy :: (a -> Bool) -> [a] -> Int
countBy pred list = length $ filter pred list

-- main = print(countBy even [1,2,3,4,6])    -- 3
-- main = print(countBy (>5) [1,5,6,7,2])    -- 2

-- 3.9 Each tuple represents a pair of scores: (score1, score2).
-- For every sublist, calculate the sum of (score1 - score2)
-- but only for tuples where the first number is strictly greater than the second.
-- If the first number is smaller or equal, skip that tuple.
-- Finally, replace each sublist with the total sum of its valid tuples.

scoreSum :: [[(Int,Int)]] -> [Int]
scoreSum list = map sum diff
      where 
            filtered = map (filter (\(a,b) -> a > b)) list
            diff = map (map (\(a,b) -> a - b )) filtered
   
-- main = print (scoreSum [[(3,1),(2,5),(6,3)],[(1,2),(4,2)],[(7,7),(8,3),(5,1)]]) -- [5,2,9]
-- main = print (scoreSum [[(10,3),(2,2),(4,1)],[],[(6,8),(7,5)]]) -- [10,0,2]

-- 3.10 Generate all perfect squares (1,4,9,16,25,...) 
-- that are less than or equal to a given limit value.

boundedSquares :: Int -> [Int]
boundedSquares n = [x ^ 2 | x <- [1..n], x ^ 2 <= n]

-- main = print(boundedSquares 30)  -- [1,4,9,16,25]
-- main = print(boundedSquares 5)   -- [1,4]
-- main = print(boundedSquares 0)   -- []


-------------------------------------------------------------------------------
-- 4. MIDTERM SAMPLE A (midtermA.hs)
-------------------------------------------------------------------------------

-- 4.1 Write a recursive function that computes a given n:
-- 1^3 + 2^3 + ... + n^3. If n is 0 or negative, return 0.

sumCubes :: Int -> Int
sumCubes n = sumCubesHelper n 0

sumCubesHelper :: Int -> Int -> Int
sumCubesHelper n acc
  | n <= 0    = acc
  | otherwise = sumCubesHelper (n-1) (acc + n*n*n)

--main = print(sumCubes 3)    -- 36   (1+8+27)
--main = print(sumCubes 5)    -- 225  (1+8+27+64+125)

-- 4.2 Write a function to compute the sum of factorials of the 
-- digits of a given number. E.g. n = 145 → 1! + 4! + 5! = 145

factorialDigitSum :: Int -> Int
factorialDigitSum n = sum (map factorial_A (toDigits (absInt n)))

absInt :: Int -> Int
absInt x 
  | x < 0     = -x
  | otherwise = x

toDigits :: Int -> [Int]
toDigits x
  | x < 10    = [x]
  | otherwise = toDigits (x `div` 10) ++ [x `mod` 10]

factorial_A :: Int -> Int
factorial_A 0 = 1
factorial_A k = k * factorial_A (k-1)

--main = print(factorialDigitSum 145) -- 145
--main = print(factorialDigitSum 123) -- 9

-- 4.3 Write a function that takes a String and returns a new String by 
-- all 'a' letter removed and uppercase characters replaced by 'X'.
-- "DanTheHandyman" -> "DnTheHndymn" (all 'a' letter removed) ->
-- "XnXheXndymn" (all capital letters change to X)

rAXhelp :: Char -> Char
rAXhelp x
  | isUpper x = 'X'
  | otherwise = x

remAaddX :: String -> String
remAaddX s = map rAXhelp (filter (\x -> x /= 'a') s)

--main = print (remAaddX "DanTheHandyman") -- "XnXheXndymn"
--main = print( remAaddX "JohnDoePlaysFootball") -- "XohnXoeXlysXootbll"

-- 4.4 Write a function that returns from a list the even numbers 
-- doubled, discarding odd numbers.

doubleEvens :: [Int] -> [Int]
doubleEvens xs = map (* 2) (filter even xs)

--main = print (doubleEvens [1 .. 10]) -- [4,8,12,16,20]
--main = print (doubleEvens [11, 13, 15]) -- []

-- 4.5 Given an n integer value, generate all (a,b,c) triples such that:
-- 1 ≤ a < b < c ≤ n and the sum of a and b should be equal to c.

sumTriples :: Int -> [(Int,Int,Int)]
sumTriples n = [ (a,b,c) | c <- [1..n], b <- [1..c-1], a <- [1..b-1], a + b == c ]

--main = print(sumTriples 3)  -- [(1,2,3)]
--main = print(sumTriples 5)  -- [(1,2,3),(1,3,4),(2,3,5),(1,4,5)]

-- 4.6 Generate an infinite list of powers of two: [1,2,4,8,16,...]
-- and return the first n elements.

firstPowersOfTwo :: Int -> [Int]
firstPowersOfTwo n = take n [2^x | x <- [0..]]

--main = print(firstPowersOfTwo 5)  -- [1,2,4,8,16]
--main = print(firstPowersOfTwo 0)  -- []

-- 4.7 Write a function that takes two lists and merges them by 
-- alternating elements of first list with elements of second 
-- list, starting with the first list.

zigzagMerge :: [a] -> [a] -> [a]
zigzagMerge [] ys = ys
zigzagMerge xs [] = xs
zigzagMerge (x:xs) (y:ys) = x : y : zigzagMerge xs ys

--main = print (zigzagMerge [1,3,5] [2,4,6]) -- [1,2,3,4,5,6]
--main = print (zigzagMerge "ace" "bdf")     -- "abcdef"

-- 4.8 Implement a function which takes a linelength (an Int) and a 
-- string, and it forms a string of length linelength by inserting 
-- spaces in front of the string. 

pushRight :: Int -> String -> String
pushRight n s 
    | n < length s = error "The text does not fit in the given linelength."
    | n >= length s = [' ' | _ <- [1..(n - length s)]] ++ s

--main = print (pushRight 6 "abc")        -- "   abc"
--main = print (pushRight 11 "crocodile") -- "  crocodile"

-- 4.9 Rotate a list to left by n positions.

rotateLeft :: [Int] -> Int -> [Int]
rotateLeft [] _ = []
rotateLeft lst n
  | n <= 0 = lst
  | n >= length lst = rotateLeft lst (n `mod` length lst)
  | otherwise = drop n lst ++ take n lst

--main = print (rotateLeft [1,2,3,4,5] 2) -- [3,4,5,1,2]
--main = print (rotateLeft [1,2,3] 4)     -- [2,3,1]

-- 4.10 Given a list of integers, produce a list where each element is 
-- the maximum value seen in the list so far (from left to right).

runningMax :: [Int] -> [Int]
runningMax [] = []
runningMax (x:xs) = x : rmAux xs x

rmAux :: [Int] -> Int -> [Int]
rmAux [] _ = []
rmAux (y:ys) curMax
  | y > curMax = y : rmAux ys y
  | otherwise  = curMax : rmAux ys curMax

--main = print (runningMax [2,1,3,2])   -- [2,2,3,3]
--main = print (runningMax [5,3,8,1,6]) -- [5,5,8,8,8]


-------------------------------------------------------------------------------
-- 5. MIDTERM SAMPLE B (midtermB.hs)
-------------------------------------------------------------------------------

-- 5.1 Write a function that takes a target digit n (from 0 to 9) 
-- and a non-negative integer, and returns how many times n 
-- appears in the digits of that number.

countN :: Int -> Int -> Int
countN n x
  | x < 10 = countHelper n x
  | otherwise = countHelper (x `rem` 10) n + countN n (div x 10)

countHelper :: Int -> Int -> Int
countHelper a b
  | a == b = 1
  | otherwise = 0

--main = print (countN 3 1234334) -- 3
--main = print (countN 5 55555555) -- 8

-- 5.2 Write a function that computes the absolute difference 
-- between the factorials of two given non-negative integers.

factorialDiff :: Int -> Int -> Int
factorialDiff a b = abs (factorial_B a - factorial_B b)

factorial_B :: Int -> Int
factorial_B 0 = 1
factorial_B n = n * factorial_B (n - 1)

--main = print (factorialDiff 5 3) -- 114
--main = print (factorialDiff 4 6) -- 696

-- 5.3 Given a list, keep elements at even positions (starts from 0) 
-- and remove elements at odd positions, and then double the elements. 

keepEvenIndices :: [Int] -> [Int]
keepEvenIndices xs = keepAux xs 0

keepAux :: [Int] -> Int -> [Int]
keepAux [] _ = []
keepAux (y:ys) i
  | even i    = (2 * y) : keepAux ys (i+1)
  | otherwise = keepAux ys (i+1)

--main = print (keepEvenIndices [1,2,3,4])        -- [2,6]
--main = print (keepEvenIndices [5,6,7,8,9])      -- [10,14,18]

-- 5.4 Create a function that takes a list of non-empty lists of integers 
-- and sums up the middle numbers. If it has even number of items 
-- then the middle item is first item of second half.

helperMid :: [Int] -> Int
helperMid ls = ls !! (length(ls) `div` 2)

midSum :: [[Int]] -> Int
midSum [] = 0
midSum (x:xs) = helperMid(x) + midSum xs

--main = print(midSum([[1,2,3],[5,7,5,3],[1]])) -- 8
--main = print(midSum([[1,2,3,4],[5,7,6,5,3],[2]])) -- 11

-- 5.5 Define a function that takes a list of lists of strings [[String]].
-- For every sublist that contains exactly two strings [sa, sb],
-- produce a string "sa sb!" by inserting a space between the 
-- two words and an exclamation mark at the end.

joinPairs :: [[String]] -> [String]
joinPairs xss = [ a ++ " " ++ b ++ "!" | [a,b] <- xss ]

--main = print (joinPairs [["hello","world"],["foo"],["good","morning"],[]]) -- ["hello world!","good morning!"]
--main = print (joinPairs [["one","two"],["three","four"]]) -- ["one two!","three four!"]

-- 5.6 Given n, generate all the numbers from 1 up to n inclusive 
-- that are divisible by 3 or by 5.

divisible35 :: Int -> [Int]
divisible35 n = [ x | x <- [1..n], x `mod` 3 == 0 || x `mod` 5 == 0 ]

--main = print(divisible35 15) -- [3,5,6,9,10,12,15]
--main = print(divisible35 4)  -- [3]

-- 5.7 Write a function that takes a list of integers and a function f.
-- Apply f to each element but only keep results that are positive.

positiveResults :: (Int -> Int) -> [Int] -> [Int]
positiveResults f xs = filter (>0) (map f xs)

--main = print(positiveResults (\x -> x - 3) [5,2,8,3])     -- [2,5]
--main = print(positiveResults (\x -> x * 2 - 10) [3,6,8])  -- [2,6]

-- 5.8 Given 2 points by (x,y) coordinates, write a function that 
-- computes the Euclidean distance between two 2D points.

distance :: (Float, Float) -> (Float, Float) -> Float
distance (x1, y1) (x2, y2) = sqrt ((x2 - x1) ^ 2 + (y2 - y1) ^ 2)

--main = print (distance (1.0, 2.0) (4.0, 6.0)) -- 5.0
--main = print (distance (0.0, 0.0) (3.0, 4.0)) -- 5.0

-- 5.9 Write a function that takes a list of integers and groups 
-- consecutive numbers that have the same parity (all even or all odd).

groupByParity :: [Int] -> [[Int]]
groupByParity [] = []
groupByParity (x:xs) = (x : takeWhile (\y -> even y == even x) xs) : groupByParity (dropWhile (\y -> even y == even x) xs)

--main = print (groupByParity [1,3,5,2,4,6,7,9]) -- [[1,3,5],[2,4,6],[7,9]]
--main = print (groupByParity [2,4,1,3,5])       -- [[2,4],[1,3,5]]

-- 5.10 Write a function that takes 3 lists and creates a new list:
-- takes all numbers from list1 and takes only those numbers 
-- from list2 that do not appear in list3.

threeLists :: [Int] -> [Int] -> [Int] -> [Int]
threeLists l m n = l ++ (m\\n)

--main = print(threeLists [1..10] [1..10] [2,4,5,3,2,5,11]) -- [1,2,3,4,5,6,7,8,9,10,1,6,7,8,9,10]
--main = print(threeLists [1..5] [2,4,5,6] [2,5,6]) --[1,2,3,4,5,4]


-------------------------------------------------------------------------------
-- 6. MIDTERM SAMPLE C (midtermC.hs)
-------------------------------------------------------------------------------

-- 6.1 Write a function that takes an integer number and creates 
-- a string from it based on each digit, if the digit:
-- - is even and less than 5 -> 'o'
-- - is even and more than 5 -> 'e'
-- - is odd and less than 5 -> 'a'
-- - is odd and more than 5 -> 'i'
-- - is 5 then 'Y'
-- - is 0 then 'X'
-- - for negative numbers return the string "Negative".

helper1 :: Int -> String
helper1 0 = "X"
helper1 n
  | n<5 && even(n) = "o"
  | n>5 && even(n) = "e"
  | n<5 && odd(n) = "a"
  | n>5 && odd(n) = "i"
  | otherwise = "Y"
  
f1_digits :: Int -> String
f1_digits n
  | n<0 = "Negative"
  | n<10 = helper1(n)
  | otherwise = f1_digits(n `div` 10) ++ helper1(n `rem` 10)
    
--main = print(f1_digits(27916))  -- "oiiae"
--main = print(f1_digits(2429738)) -- "oooiiae"
--main = print(f1_digits(-17882))   -- "Negative"

-- 6.2 Write a function that removes all empty sublists 
-- from a list of lists. Recursion must be used.

removeEmpty :: [[Int]] -> [[Int]]
removeEmpty [] = []
removeEmpty ([] : xs) = removeEmpty xs
removeEmpty (x : xs) = x : removeEmpty xs

--main = print (removeEmpty [[1, 2], [], [3, 4, 5], [], [], [6]]) -- [[1,2],[3,4,5],[6]]
--main = print (removeEmpty [[], [], []]) -- []

-- 6.3 Write a function that takes a list of integer pairs and returns 
-- a list of triple (3-tuple) of integers. The first and second elements of 
-- the resulted tuple should be the original two integers. 
-- The third element depends on whether the numbers are odd or not. 
-- If both numbers are odd, return their sum as the third element. 
-- Otherwise, return their product as the third element.

computePair :: (Int, Int) -> (Int, Int, Int)
computePair (x, y)
  | odd x && odd y = (x, y, x + y)
  | otherwise = (x, y, x * y)
 
--main = print (computePair (3, 5)) -- (3,5,8)
--main = print (computePair (6, 6)) -- (6,6,36)

pairs :: [(Int, Int)] -> [(Int, Int, Int)]
pairs x = map computePair x

--main = print (pairs [(1,1),(2,2),(3,3)]) -- [(1,1,2),(2,2,4),(3,3,6)]
--main = print (pairs []) -- []

-- 6.4 Write a function that takes two lists of integers and a target value, 
-- and returns all pairs (x, y) such that x is from the first list, 
-- y is from the second list, and x + y should be equal to the target.

sumPairsTo :: [Int] -> [Int] -> Int -> [(Int, Int)]
sumPairsTo xs ys target = [(x, y) | x <- xs, y <- ys, x + y == target]

--main = print (sumPairsTo [1, 2, 3] [4, 5, 6] 7) -- [(1,6),(2,5),(3,4)]
--main = print (sumPairsTo [0, -1, 2] [1, 2, 3] 1) -- [(0,1),(-1,2)]

-- 6.5 Given a list and two functions fEven and fOdd, apply fEven 
-- to elements at even indices (starts from 0) and fOdd to elements 
-- at odd indices. Return the list obtained.

applyByIndex :: (a -> a) -> (a -> a) -> [a] -> [a]
applyByIndex fEven fOdd xs = aux xs 0 fEven fOdd

aux :: [a] -> Int -> (a -> a) -> (a -> a) -> [a]
aux [] _ _ _ = []
aux (y:ys) i fEven fOdd
  | even i    = fEven y : aux ys (i+1) fEven fOdd
  | otherwise = fOdd  y : aux ys (i+1) fEven fOdd

--main = print (applyByIndex (*2) (+1) [1,2,3,4,5]) -- [2,3,6,5,10]
--main = print (applyByIndex (++ "!") (++ "?") ["a","b","c"]) -- ["a!","b?","c!"]

-- 6.6 Create a function taking a list of lists of characters and 
-- giving a string: combination of first and last characters 
-- of each list joined together like: 
-- [['a','b','x','c'],['d','e','f'],['g','h']] -> "acdfgh"

charJoin :: [[Char]] -> String
charJoin [] = ""
charJoin (x:xs) = [head(x)] ++ [last(x)] ++ charJoin(xs)

--main = print(charJoin [['a','b','c'],['d','e','f'],['g','h']]) -- "acdfgh"
--main = print(charJoin(["John","Doe"])) -- "JnDe"

-- 6.7 Create a function that takes a list of 3 Int tuples and 
-- flattens them to a single list.

tLhelp :: (Int,Int,Int) -> [Int]
tLhelp (a,b,c) = [a,b,c]

tupList :: [(Int,Int,Int)] -> [Int]
tupList list = concat (map tLhelp list)

--main = print(tupList [(1,2,3),(4,5,6),(7,8,9)]) -- [1,2,3,4,5,6,7,8,9]
--main = print(tupList [(1,0,1),(2,0,2)]) -- [1,0,1,2,0,2]

-- 6.8 Write a function to remove every k-th element from a list, 
-- counting starts from 1.

removeEvery :: Int -> [a] -> [a]
removeEvery k xs
  | k <= 0    = xs
  | otherwise = [ x | (x,i) <- zip xs [1..], i `mod` k /= 0 ]

--main = print(removeEvery 2 [1,2,3,4,5,6])     -- [1,3,5]
--main = print(removeEvery 3 [10.0,20.0,30.0,40.0,50.0,60.0,70.0])  -- [10.0,20.0,40.0,50.0,70.0]

-- 6.9 Write a function that applies a list of functions from 
-- left-to-right to a value x.

applyAll :: [a -> a] -> a -> a
applyAll fs x = foldl (\v f -> f v) x fs

--main = print(applyAll [(+1),(*2), (*10)] 3)       -- 80
--main = print(applyAll [reverse, (drop 1)] "abcd") -- "cba"

-- 6.10 Given a list of integers and two integer limits, low and high,
-- remove all elements that are within the range [low..high].

dropInRange :: [Int] -> Int -> Int -> [Int]
dropInRange xs low high = [x | x <- xs, x < low || x > high]

--main = print (dropInRange [1,5,10,15,20] 5 15) -- [1,20]
--main = print (dropInRange [2,3,4,5,6] 3 5) -- [2,6]