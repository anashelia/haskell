{-# LANGUAGE ParallelListComp #-}
import Data.Char
import Data.List


main :: IO ()


-----------------------------------------------------------

{- 1.
Write a recursive function that computes a given n:
1^3 + 2^3 + ... + n^3. If n is 0 or negative, return 0.
The function has to be recursive function, generator 
solution only gets half of the points.
-}

sumCubes :: Int -> Int
sumCubes n = sumCubesHelper n 0

sumCubesHelper :: Int -> Int -> Int
sumCubesHelper n acc
  | n <= 0    = acc
  | otherwise = sumCubesHelper (n-1) (acc + n*n*n)

--main = print(sumCubes 3)    -- 36   (1+8+27)
--main = print(sumCubes 5)    -- 225  (1+8+27+64+125)
--main = print(sumCubes 0)    -- 0
--main = print(sumCubes (-4)) -- 0

-----------------------------------------------------------

{- 2.
Write a function to compute the sum of factorials of the 
digits of a given number.
E.g. n = 145 → 1! + 4! + 5! = 145
-}

factorialDigitSum :: Int -> Int
factorialDigitSum n = sum (map factorial (toDigits (absInt n)))

absInt :: Int -> Int
absInt x 
  | x < 0     = -x
  | otherwise = x

toDigits :: Int -> [Int]
toDigits x
  | x < 10    = [x]
  | otherwise = toDigits (x `div` 10) ++ [x `mod` 10]

factorial :: Int -> Int
factorial 0 = 1
factorial k = k * factorial (k-1)

--main = print(factorialDigitSum 145) -- 145
--main = print(factorialDigitSum 123) -- 9
--main = print(factorialDigitSum 5)   -- 120
--main = print(factorialDigitSum 999) -- 1088640
--main = print(factorialDigitSum 0)   -- 1

-----------------------------------------------------------

{- 3.
Write a function that takes a String and returns a new String by 
all 'a' letter removed and uppercase characters replaced by 'X'.
"DanTheHandyman" -> "DnTheHndymn" (all 'a' letter removed) ->
"XnXheXndymn" (all capital letters change to X)
-}

rAXhelp :: Char -> Char
rAXhelp x
  | isUpper x = 'X'
  | otherwise = x

remAaddX :: String -> String
remAaddX s = map rAXhelp (filter (\x -> x /= 'a') s)

--main = print (remAaddX "DanTheHandyman") -- "XnXheXndymn"
--main = print( remAaddX "JohnDoePlaysFootball") -- "XohnXoeXlysXootbll"
--main = print (remAaddX "Aaaa bbb ccc") -- "X bbb ccc"
--main = print (remAaddX "   ") -- "   "
--main = print (remAaddX "") -- ""

-----------------------------------------------------------

{- 4.
Write a function that returns from a list the even numbers 
doubled, discarding odd numbers.
-}

doubleEvens :: [Int] -> [Int]
doubleEvens xs = map (* 2) (filter even xs)

--main = print (doubleEvens [1 .. 10]) -- [4,8,12,16,20]
--main = print (doubleEvens [11, 13, 15]) -- []
--main = print (doubleEvens [2, 4, 6, 8, 21, 11, 10]) -- [4,8,12,16,20]
--main = print (doubleEvens []) -- []

-----------------------------------------------------------

{- 5.
Given an n integer value, generate all (a,b,c) triples such that:
1 ≤ a < b < c ≤ n and the sum of a and b should be equal to c.
-}

sumTriples :: Int -> [(Int,Int,Int)]
sumTriples n = [ (a,b,c) | c <- [1..n], b <- [1..c-1], a <- [1..b-1], a + b == c ]

--main = print(sumTriples 3)  -- [(1,2,3)]
--main = print(sumTriples 5)  -- [(1,2,3),(1,3,4),(2,3,5),(1,4,5)]
--main = print(sumTriples 1)  -- []
--main = print(sumTriples 2)  -- []
--main = print(sumTriples 8)  
-- [(1,2,3),(1,3,4),(2,3,5),(1,4,5),(2,4,6),(1,5,6),(3,4,7),(2,5,7),(1,6,7),(3,5,8),(2,6,8),(1,7,8)]

-----------------------------------------------------------

{- 6.
Generate an infinite list of powers of two: [1,2,4,8,16,...]
and return the first n elements.
n = 5 -> [1,2,4,8,16]
n = 0 -> []
-}

firstPowersOfTwo :: Int -> [Int]
firstPowersOfTwo n = take n [2^x | x <- [0..]]

--main = print(firstPowersOfTwo 5)  -- [1,2,4,8,16]
--main = print(firstPowersOfTwo 0)  -- []
--main = print(firstPowersOfTwo 8)  -- [1,2,4,8,16,32,64,128]
--main = print(firstPowersOfTwo 1)  -- [1]
--main = print(firstPowersOfTwo 3)  -- [1,2,4]

-----------------------------------------------------------

{- 7.
Write a function that takes two lists and merges them by 
alternating elements of first list with elements of second 
list, starting with the first list.
-}

zigzagMerge :: [a] -> [a] -> [a]
zigzagMerge [] ys = ys
zigzagMerge xs [] = xs
zigzagMerge (x:xs) (y:ys) = x : y : zigzagMerge xs ys

--main = print (zigzagMerge [1,3,5] [2,4,6]) -- [1,2,3,4,5,6]
--main = print (zigzagMerge "ace" "bdf")     -- "abcdef"
--main = print (zigzagMerge [1.1,2.2] [3.5,4.8,5.3,6.1]) -- [1.1,3.5,2.2,4.8,5.3,6.1]
--main = print (zigzagMerge [] [7,8,9])      -- [7,8,9]
--main = print (zigzagMerge [1..10] [])  -- [1,2,3,4,5,6,7,8,9,10]

-----------------------------------------------------------

{- 8.
Implement a function which takes a linelength (an Int) and a 
string, and it forms a string of length linelength by inserting 
spaces in front of the string. 
E.g. linelength = 11,  str = "crocodile" -> "  crocodile"
If the given string is longer than the specified linelength, 
the function should raise an error with the message:
"The text does not fit in the given linelength."
-}

pushRight :: Int -> String -> String
pushRight n s 
    | n < length s = error "The text does not fit in the given linelength."
    | n >= length s = [' ' | n<- [1..(n - length s)]] ++ s

--main = print (pushRight 6 "abc")        -- "   abc"
--main = print (pushRight 11 "crocodile") -- "  crocodile"
--main = print (pushRight 7 "cat")        -- "    cat"
--main = print (pushRight 1 "hello")      -- The text does not fit in the given linelength.

-----------------------------------------------------------

{- 9.
Rotate a list to left by n positions.
E.g.: rotateLeft [1,2,3,4,5] 2 -> [3,4,5,1,2]
      rotateLeft [1,2,3] 4 -> [2,3,1] (wraps around)
      rotateLeft [] 3 -> []
-}

rotateLeft :: [Int] -> Int -> [Int]
rotateLeft [] _ = []
rotateLeft lst n
  | n <= 0 = lst
  | n >= length lst = rotateLeft lst (n `mod` length lst)
  | otherwise = drop n lst ++ take n lst

--main = print (rotateLeft [1,2,3,4,5] 2) -- [3,4,5,1,2]
--main = print (rotateLeft [1,2,3] 4)     -- [2,3,1]
--main = print (rotateLeft [] 3)          -- []
--main = print (rotateLeft [1] 10)        -- [1]

-----------------------------------------------------------

{- 10.
Given a list of integers, produce a list where each element is 
the maximum value seen in the list so far (from left to right).
E.g.: [2,1,3,2] -> [2,2,3,3]
-}

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
--main = print (runningMax [1,2,3,4,5]) -- [1,2,3,4,5]
--main = print (runningMax [5,4,3,2,1]) -- [5,5,5,5,5]
--main = print (runningMax [])          -- []
--main = print (runningMax [8])         -- [8]

-----------------------------------------------------------