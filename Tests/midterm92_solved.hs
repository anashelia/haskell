{-# LANGUAGE ParallelListComp #-}
import Data.Char
import Data.List


main :: IO ()

-- WRITE YOUR NAME AND NEPTUN CODE HERE
-----------------------------------------------------------

{- 1.
Given a non-negative integer n, compute the product of its non-zero digits.
If n = 0 return 0. Examples: 105 -> 1*5 = 5, 240 -> 2*4 = 8, 0 -> 0
-}

prodNonZeroDigits :: Int -> Int
prodNonZeroDigits 0 = 0  -- needed product returns 1
prodNonZeroDigits n = product (map digitToInt (filter (\x -> digitToInt x /= 0) (show n)))

-- main = print (prodNonZeroDigits 105) -- 5
-- main = print (prodNonZeroDigits 240) -- 8
-- main = print (prodNonZeroDigits 0)   -- 0
-- main = print (prodNonZeroDigits 123045) -- 120
--main = print (prodNonZeroDigits 7) -- 7

-----------------------------------------------------------

{- 2. 
Count how many times a digit d (0..9) appears in a non-negative 
integer n. E.g.: countDigit 12231 2 -> 2
-}

countDigit :: Int -> Int -> Int
countDigit n i = length $ filter (\x -> digitToInt x == i) (show n)

-- main = print (countDigit 12231 2)  -- 2
-- main = print (countDigit 0 0)      -- 1
-- main = print (countDigit 123418 5) -- 0

-----------------------------------------------------------

{- 3.
Write a function that checks if the first list is 
a prefix of the second list. [] is a prefix of any list.
-}

isPrefix :: [Int] -> [Int] -> Bool
isPrefix pref list = pref == take (length pref) list

-- main = print $ isPrefix [1, 2] [1, 2, 3, 4] -- True
-- main = print $ isPrefix [2, 3] [1, 2, 3, 4] -- False
-- main = print $ isPrefix [] [1, 2, 3] -- True
-- main = print $ isPrefix [1, 2, 3, 4] [1, 2] -- False

-----------------------------------------------------------

{- 4.
Write a function that takes a predicate, a function, and a list. 
It applies the function only to elements that satisfy the predicate.
-}

help :: (a -> Bool) -> (a -> a) -> a -> a
help pred f x
   | pred x = f x
   | otherwise = x

filterApply :: (a -> Bool) -> (a -> a) -> [a] -> [a]
filterApply  pred func list = map (help pred func) list

-- main = print (filterApply (>5) (*2) [1,6,3,8,4])     -- [1,12,3,16,4]
-- main = print (filterApply even (+10) [1,2,3,4,5])    -- [1,12,3,14,5]
-- main = print (filterApply (const True) (*3) [2,4])   -- [6,12]
-- main = print (filterApply even (+10) []) -- []

-----------------------------------------------------------

{- 5.
Make triples (i,a_i,b_i) from two lists, pair elements of same 
position and annotate by the index. Truncate to the shorter list.
-}

indexedTriples :: [a] -> [b] -> [(Int,a,b)]
indexedTriples list1 list2 = [(i,x,y) | i <- [0..] | x <- list1 | y <- list2]

-- main = print (indexedTriples ["x","y"] [10,20])   -- [(0,"x",10),(1,"y",20)]
-- main = print (indexedTriples [1,2,3] ['a','b'])   -- [(0,1,'a'),(1,2,'b')]
-- main = print (indexedTriples ([]::[Int]) [1,2,3]) -- []
-- main = print (indexedTriples [1,2,3]([]::[Int]))  -- []
-- main = print (indexedTriples ([]::[Int]) ([]::[Int])) -- []
-- main = print (indexedTriples [1..10] ['a','b'])   -- [(0,1,'a'),(1,2,'b')]

-----------------------------------------------------------

{- 6.
Write a function that takes two lists of strings and joins 
the strings of same positions together and creates a new 
string, assume both lists always have same number of strings.
-}

stJoin :: [String] -> [String] -> String
stJoin list1 list2 = concat [a ++ b | (a,b) <- zip list1 list2]

-- main = print (stJoin ["John "," a "," man"] ["is","good"," !"]) -- "John is a good man !"
-- main = print (stJoin ["Peter ","picked ","pack ", "pickled "] ["Piper ","a ","of ","peppers."]) 
--  "Peter Piper picked a pack of pickled peppers."
-- main = print (stJoin [] []) -- ""

-----------------------------------------------------------

{- 7.
Given list of (name, score), return the name with highest score.
If the list is empty, return the error "Empty list!".
-}

bestStudent :: [(String,Int)] -> String
bestStudent [] = error "Empty list!"
bestStudent list = [name | (name, s) <- list, s == maxScore] !! 0  -- !!0 needed in case many have max score
      where 
            maxScore = maximum $ snd $ unzip list

-- main = print(bestStudent [("Ana",90),("Bob",85)])       -- "Ana"
-- main = print(bestStudent [("Chris",70)])                -- "Chris"
-- main = print(bestStudent [("X",100),("Y",100),("Z",100)])         -- "X"
-- main = print(bestStudent [("A",1),("B",2),("C",3),("E",1),("D",2)]) -- "C"
-- main = print(bestStudent [])  -- "Empty list!"

-----------------------------------------------------------

{- 8.
Write a function that counts how many elements in a list 
satisfy a given predicate p.
-}

countBy :: (a -> Bool) -> [a] -> Int
countBy pred list = length $ filter pred list

-- main = print(countBy even [1,2,3,4,6])    -- 3
-- main = print(countBy (>5) [1,5,6,7,2])    -- 2
-- main = print(countBy (const True) [])     -- 0
-- main = print(countBy (\s -> length s>3) ["hi","haskell","x"]) -- 1

-----------------------------------------------------------

{- 9.
Each tuple represents a pair of scores: (score1, score2).
For every sublist, calculate the sum of (score1 - score2)
but only for tuples where the first number is strictly greater than the second.
If the first number is smaller or equal, skip that tuple.
Finally, replace each sublist with the total sum of its valid tuples.
E.g.: [[(3,1),(2,5),(6,3)],[(1,2),(4,2)],[(7,7),(8,3),(5,1)]] -> 
      [(3-1)+(6-3), (4-2), (8-3)+(5-1)] = [5,2,9]
-}

scoreSum :: [[(Int,Int)]] -> [Int]
scoreSum list = map sum diff
      where 
            filtered = map (filter (\(a,b) -> a > b)) list
            diff = map (map (\(a,b) -> a - b )) filtered
   
-- main = print (scoreSum [[(3,1),(2,5),(6,3)],[(1,2),(4,2)],[(7,7),(8,3),(5,1)]]) -- [5,2,9]
-- main = print (scoreSum [[(10,3),(2,2),(4,1)],[],[(6,8),(7,5)]]) -- [10,0,2]
-- main = print (scoreSum [[(1,2),(3,4)],[(5,5)],[(9,1),(3,2),(0,0)],[],[]]) -- [0,0,9,0,0]
-- main = print (scoreSum [[],[(5,5)],[(1,2),(0,0)]]) -- [0,0,0]
-- main = print (scoreSum []) -- []

-----------------------------------------------------------

{- 10.
Generate all perfect squares (1,4,9,16,25,...) 
that are less than or equal to a given limit value.
limit = 30 -> [1,4,9,16,25]
limit = 5  -> [1,4]
limit = 0  -> []
-}

boundedSquares :: Int -> [Int]
boundedSquares n = [x ^ 2 | x <- [1..n], x ^ 2 <= n]

-- main = print(boundedSquares 30)  -- [1,4,9,16,25]
-- main = print(boundedSquares 5)   -- [1,4]
-- main = print(boundedSquares 1)   -- [1]
-- main = print(boundedSquares 0)   -- []
-- main = print(boundedSquares 100) -- [1,4,9,16,25,36,49,64,81,100]

-----------------------------------------------------------