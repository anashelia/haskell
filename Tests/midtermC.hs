{-# LANGUAGE ParallelListComp #-}
import Data.Char
import Data.List


main :: IO ()


-----------------------------------------------------------

{- 1.
Write a function that takes an integer number and creates 
a string from it based on each digit, if the digit:
- is even and less than 5 -> 'o'
- is even and more than 5 -> 'e'
- is odd and less than 5 -> 'a'
- is odd and more than 5 -> 'i'
- is 5 then 'Y'
- is 0 then 'X'
- for negative numbers return the string "Negative".
-}

helper1 :: Int -> String
helper1 0 = "X"
helper1 n
  | n<5 && even(n) = "o"
  | n>5 && even(n) = "e"
  | n<5 && odd(n) = "a"
  | n>5 && odd(n) = "i"
  | otherwise = "Y"
  

f1 :: Int -> String
f1 n
  | n<0 = "Negative"
  | n<10 = helper1(n)
  | otherwise = f1(n `div` 10) ++ helper1(n `rem` 10)
    
--main = print(f1(27916))  -- "oiiae"
--main = print(f1(2429738)) -- "oooiiae"
--main = print(f1(-17882))   -- "Negative"
--main = print(f1(1)) -- "a"
--main = print(f1(0)) -- "X"

-----------------------------------------------------------

{- 2.
Write a function that removes all empty sublists 
from a list of lists. Recursion must be used.
Solution with generator gets half points.
-}

removeEmpty :: [[Int]] -> [[Int]]
removeEmpty [] = []
removeEmpty ([] : xs) = removeEmpty xs
removeEmpty (x : xs) = x : removeEmpty xs

--removeEmpty ls = filter (\x -> not (null x)) ls  

--main = print (removeEmpty [[1, 2], [], [3, 4, 5], [], [], [6]]) -- [[1,2],[3,4,5],[6]]
--main = print (removeEmpty [[], [], []]) -- []
--main = print (removeEmpty [[1, 2, 3], [4, 5], [6]]) -- [[1,2,3],[4,5],[6]]

-----------------------------------------------------------

{- 3.
Write a function that takes a list of integer pairs and returns 
a list of triple (3-tuple) of integers. The first and second elements of 
the resulted tuple should be the original two integers. 
The third element depends on whether the numbers are odd or not. 
If both numbers are odd, return their sum as the third element. 
Otherwise, return their product as the third element.
-}

computePair :: (Int, Int) -> (Int, Int, Int)
computePair (x, y)
  | odd x && odd y = (x, y, x + y)
  | otherwise = (x, y, x * y)
 
--main = print (computePair (3, 5)) -- (3,5,8)
--main = print (computePair (6, 6)) -- (6,6,36)
--main = print (computePair (2, 3)) -- (2,3,6)
--main = print (computePair (0, 1)) -- (0,1,0)

pairs :: [(Int, Int)] -> [(Int, Int, Int)]
pairs x = map computePair x

--main = print (pairs [(1,1),(2,2),(3,3)]) -- [(1,1,2),(2,2,4),(3,3,6)]
--main = print (pairs []) -- []
--main = print (pairs [(1,3),(4,2),(8,3),(5,1),(0,0),(1,0),(2,0)])
-- [(1,3,4),(4,2,8),(8,3,24),(5,1,6),(0,0,0),(1,0,0),(2,0,0)]

-----------------------------------------------------------

{- 4.
Write a function that takes two lists of integers and a target value, 
and returns all pairs (x, y) such that x is from the first list, 
y is from the second list, and x + y should be equal to the target.
-}

sumPairsTo :: [Int] -> [Int] -> Int -> [(Int, Int)]
sumPairsTo xs ys target = [(x, y) | x <- xs, y <- ys, x + y == target]

--main = print (sumPairsTo [1, 2, 3] [4, 5, 6] 7) -- [(1,6),(2,5),(3,4)]
--main = print (sumPairsTo [0, -1, 2] [1, 2, 3] 1) -- [(0,1),(-1,2)]
--main = print (sumPairsTo [10, 20] [30, 40] 100) -- []
--main = print (sumPairsTo [] [] 0) -- []

-----------------------------------------------------------

{- 5.
Given a list and two functions fEven and fOdd, apply fEven 
to elements at even indices (starts from 0) and fOdd to elements 
at odd indices. Return the list obtained.
fEven (*2), fOdd (+1) on [1,2,3,4,5]->[1*2,2+1,3*2,4+1,5*2] = [2,3,6,5,10]
-}

applyByIndex :: (a -> a) -> (a -> a) -> [a] -> [a]
applyByIndex fEven fOdd xs = aux xs 0 fEven fOdd

aux :: [a] -> Int -> (a -> a) -> (a -> a) -> [a]
aux [] _ _ _ = []
aux (y:ys) i fEven fOdd
  | even i    = fEven y : aux ys (i+1) fEven fOdd
  | otherwise = fOdd  y : aux ys (i+1) fEven fOdd

--main = print (applyByIndex (*2) (+1) [1,2,3,4,5]) -- [2,3,6,5,10]
--main = print (applyByIndex (++ "!") (++ "?") ["a","b","c"]) -- ["a!","b?","c!"]
--main = print (applyByIndex (+10) (*5) [0.0,1.1,2.0,3.5]) -- [10.0,5.5,12.0,17.5]
--main = print (applyByIndex (+2) (*3) []) -- []

-----------------------------------------------------------

{- 6.
Create a function taking a list of lists of characters and 
giving a string: combination of first and last characters 
of each list joined together like: 
[['a','b','x','c'],['d','e','f'],['g','h']] -> "acdfgh"
Assume sublists are not empty and have at least 2 characters.
-}

charJoin :: [[Char]] -> String
charJoin [] = ""
charJoin (x:xs) = [head(x)] ++ [last(x)] ++ charJoin(xs)

--main = print(charJoin [['a','b','c'],['d','e','f'],['g','h']]) -- "acdfgh"
--main = print(charJoin(["John","Doe"])) -- "JnDe"
--main = print(charJoin(["ABCD","FGHJK", "qwerty", "computer", "XY"])) -- "ADFKqycrXY"
--main = print(charJoin([])) -- ""

-----------------------------------------------------------

{- 7.
Create a function that takes a list of 3 Int tuples and 
flattens them to a single list.
-}

tLhelp :: (Int,Int,Int) -> [Int]
tLhelp (a,b,c) = [a,b,c]

tupList :: [(Int,Int,Int)] -> [Int]
tupList list = concat (map tLhelp list)

--main = print(tupList [(1,2,3),(4,5,6),(7,8,9)]) -- [1,2,3,4,5,6,7,8,9]
--main = print(tupList [(1,0,1),(2,0,2)]) -- [1,0,1,2,0,2]
--main = print(tupList [(10,1,12)]) -- [10,1,12]
--main = print(tupList []) -- []

-----------------------------------------------------------

{- 8.
Write a function to remove every k-th element from a list, 
counting starts from 1.
-}

removeEvery :: Int -> [a] -> [a]
removeEvery k xs
  | k <= 0    = xs
  | otherwise = [ x | (x,i) <- zip xs [1..], i `mod` k /= 0 ]

--main = print(removeEvery 2 [1,2,3,4,5,6])     -- [1,3,5]
--main = print(removeEvery 3 [10.0,20.0,30.0,40.0,50.0,60.0,70.0])  -- [10.0,20.0,40.0,50.0,70.0]
--main = print(removeEvery 1 [1,2,3])           -- []
--main = print(removeEvery 4 "haskellrocks!")   -- "hasellock!"

-----------------------------------------------------------

{- 9.
Write a function that applies a list of functions from 
left-to-right to a value x.
E.g.: [(+1), (*2), (*10)] 3=(*10) ((*2) ((+1) 3))=((3+1)*2)*10)=80
-}

applyAll :: [a -> a] -> a -> a
applyAll fs x = foldl (\v f -> f v) x fs

--main = print(applyAll [(+1),(*2), (*10)] 3)       -- 80
--main = print(applyAll [reverse, (drop 1)] "abcd") -- "cba"
--main = print(applyAll [(\n -> n*n), (+3)] 2)      -- 7
--main = print(applyAll [] 42)                      -- 42

-----------------------------------------------------------

{- 10.
Given a list of integers and two integer limits, low and high,
remove all elements that are within the range [low..high].
[1,5,10,15,20], low=5, high=15  -> [1,20]
[2,3,4,5,6], low=3, high=5      -> [2,6]
[10,20,30], low=100, high=200   -> [10,20,30]
-}

dropInRange :: [Int] -> Int -> Int -> [Int]
dropInRange xs low high = [x | x <- xs, x < low || x > high]

--main = print (dropInRange [1,5,10,15,20] 5 15) -- [1,20]
--main = print (dropInRange [2,3,4,5,6] 3 5) -- [2,6]
--main = print(dropInRange [10, 330, 20, 202, 21, 66, 30, 150, 110, 11] 100 200) -- [10,330,20,202,21,66,30,11]
--main = print (dropInRange [] 1 10) -- []
--main = print(dropInRange [5,9] 6 8) -- [5,9]

-----------------------------------------------------------