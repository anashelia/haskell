{-# LANGUAGE ParallelListComp #-}
import Data.Char
import Data.List


main :: IO ()


-- 1. *****************************************************
-- Create a function that takes an integer and counts 
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


-- 2. *****************************************************
-- Given an n, and a list of words. Write a function that 
-- keeps any word with n or more characters. 

filterWords :: Int -> [String] -> [String]
filterWords n words = filter (\word -> length word >= n) words

--main = print (filterWords 5 ["hello", "to", "the", "world", "of", "Haskell"]) -- ["hello","world","Haskell"]
--main = print (filterWords 3 ["Functional", "Programming", "is", "very", "cool"]) -- ["Functional","Programming","very","cool"]
--main = print (filterWords 6 ["I", "love", "AI"]) -- []
--main = print (filterWords 2 []) -- []


-- 3. *****************************************************
-- Write a function that takes two lists of strings and returns 
-- a list of all pairs (x, y) such that y is the inverted x,
-- where x is from the first list and y is from the second one.

mirrorPairs :: [String] -> [String] -> [(String, String)]
mirrorPairs xs ys = [(x, y) | x <- xs, y <- ys, reverse x == y]

--main = print (mirrorPairs ["abc", "123", "live", "a"] ["cba", "321", "evil", "b", "123"]) -- [("abc","cba"),("123","321"),("live","evil")]
--main = print (mirrorPairs ["hello", "world"] ["olleh", "dlrow"]) -- [("hello","olleh"),("world","dlrow")]
--main = print (mirrorPairs ["foo", "bar"] ["bax", "rab", "qux", "rab", "rab", ""]) -- [("bar","rab"),("bar","rab"),("bar","rab")]
--main = print (mirrorPairs ["", "bar"] [""]) -- [("","")]
--main = print (mirrorPairs [] []) -- []
--main = print (mirrorPairs ["abc", "123", "", "abc", "123"] ["cba", "d", "123", "321", "evil", "321"]) 
--     [("abc","cba"),("123","321"),("123","321"),("abc","cba"),("123","321"),("123","321")]


-- 4. *****************************************************
-- Create a function that takes a list of list of integers 
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
--main = print(fstEven([])) -- []
--main = print(fstEven([[1,2,2],[2,2,7],[1],[],[5,7],[],[],[8]])) -- [[2,2,7],[8]]


-- 5. *****************************************************
-- Write a function that takes a tuple of 2 non-empty lists and 
-- creates a new Integer list made by joining first item of 2nd list
-- with everything except the first item of 1st list.
-- e.g. ([1,2,3,4],[5,6,7,8]) -> [5,2,3,4]

swapTup :: ([Int],[Int]) -> [Int]
swapTup (a,b) = [head b] ++ tail a

--main = print(swapTup ([1,2,3,4],[5,6,7,8])) -- [5,2,3,4]
--main = print(swapTup ([1..10],[100,99..90])) -- [100,2,3,4,5,6,7,8,9,10]
--main = print(swapTup ([2],[1])) -- [1]


-- 6. *****************************************************
-- Given a list of (a,b) pairs, return the list of a+b for each tuple.

sumPairs :: [(Int,Int)] -> [Int]
sumPairs ps = map (\(a,b) -> a + b) ps

--main = print(sumPairs [(1,2),(3,4)])      -- [3,7]
--main = print(sumPairs [])                  -- []
--main = print(sumPairs [(10,-3),(5,5),(1,1),(-1,1),(3,3)]) -- [7,10,2,0,6]
--main = print(sumPairs [(-1,-2),(0,0)])    -- [-3,0]


-- 7. *****************************************************
-- Given a function f and a list, apply f to each consecutive x,y,z of the list
-- and return the result. If fewer than 3 elements, return [].
-- E.g. f x y z = x+y+z  [1,2,3,4,5] -> [1+2+3,2+3+4,3+4+5] = [6,9,12]

applyTriples :: (a -> a -> a -> b) -> [a] -> [b]
applyTriples _ [] = []
applyTriples _ [_] = []
applyTriples _ [_,_] = []
applyTriples f (x:y:z:xs) = f x y z : applyTriples f (y:z:xs)

-- helper example function used in tests
sum3 :: Int -> Int -> Int -> Int
sum3 a b c = a + b + c

prod3 :: Int -> Int -> Int -> Int
prod3 a b c = a * b * c

max3 :: Int -> Int -> Int -> Int
max3 a b c = max a (max b c)

--main = print (applyTriples sum3 [1,2,3,4,5])   -- [6,9,12]
--main = print (applyTriples sum3 [1])           -- []
--main = print (applyTriples sum3 [5,10,15])     -- [30]
--main = print (applyTriples sum3 [1,2])         -- []
--main = print (applyTriples sum3 [-1,-2,-3,-4]) -- [-6,-9]
--main = print (applyTriples sum3 [])            -- []
--main = print (applyTriples prod3 [2,3,4,5])    -- [24,60]
--main = print (applyTriples max3 [1,7,3,5])     -- [7,7]


-- 8. *****************************************************
-- Given a predicate p and a list of integers,
-- return the sum of the squares of the elements that satisfy p.
-- Use higher-order functions (no recursion, no generator).
-- p x = even x     [1,2,3,4,5] -> 2^2 + 4^2 = 20

sumSquaresIf :: (Int -> Bool) -> [Int] -> Int
sumSquaresIf p xs = sum (map (\x -> x * x) (filter p xs))

--main = print(sumSquaresIf (\x -> even x) [1,2,3,4,5]) -- 20
--main = print(sumSquaresIf (\x -> x > 10) [5,10,11,3,12]) -- 265
--main = print(sumSquaresIf (\x -> x `mod` 3 == 1) [1..10]) -- 166
--main = print(sumSquaresIf even [1]) -- 0
--main = print(sumSquaresIf odd []) -- 0


-- 9. *****************************************************
-- Write a function that takes a number and returns all pairs 
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
--main = print (digitPairs 4268)
--  [(4,2),(4,6),(4,8),(2,4),(2,6),(2,8),(6,4),(6,2),(6,8),(8,4),(8,2),(8,6)]


-- 10. *****************************************************
-- Define a function to count how many words are in a dictionary.
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
--main = print (countWords [] [("apple",1),("banana",2)])  -- 0
--main = print (countWords ["tree","leaf","branch"] []) -- 0
--main = print (countWords [] []) -- 0

-- *********************************************************