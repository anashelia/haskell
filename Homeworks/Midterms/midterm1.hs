-- 1. --------------------------------
-- Write a function that takes a non-negative integer and returns 
-- the number of odd digits in that integer.
-- If the input is negative, return an error message 
-- as a string "Negative number error".
-- Example: 
-- countOdds 1234 -- 2
-- countOdds 987654321 -- 5

eachDigit :: Int -> [Int]
eachDigit 0 = []
eachDigit n = eachDigit (n `div` 10) ++ [n `rem` 10]

countOdds :: Int -> Int
countOdds x
    | x < 0     = error "Negative number error"
    | otherwise = length [d | d <- eachDigit x, odd d]

-- 2. --------------------------------
-- Write a function that takes two lists of integers 
-- and returns a list of integers that appear in both lists, 
-- ensuring each element appears only once in the result, 
-- regardless of how many times it appears in the input lists.
commonElements :: [Int] -> [Int] -> [Int]
commonElements lst1 lst2 = [ i | i <- lst1, j <- lst2, i==j]

-- 3. --------------------------------
-- Write a function prefixes that takes a list and returns 
-- a list of all prefixes of the original list.
-- Example:
-- prefixes [1, 2, 3]  output: [[1], [1, 2], [1, 2, 3]]
prefixes :: [a] -> [[a]]
prefixes lst = [take n lst| n <- [1.. length lst]]

-- 4. --------------------------------
-- Write a function which takes input of 2 matrices (list of lists)
-- of the same size, and output the sum of the 
-- squared differences of corresponding elements.
-- Example:
-- [[1, 2]
--  [4, 5]]
-- [[2, 3]
--  [5, 6]]
-- = (2-1)^2 + (3-2)^2 + (5-4)^2 + (6-5)^2= 4
ssd :: [[Int]] -> [[Int]] -> Int
ssd a b = sum [ (x - y)^2 | (row1, row2) <- zip a b, (x, y) <- zip row1 row2 ]

-- 5. --------------------------------
-- Write a function that takes a list of tuples of integers
-- and returns the sum of all the elements in all the tuples.
sumPairs :: [(Int, Int)] -> Int
sumPairs lst = sum (map (\(a,b) -> a+b) lst)

-- 6. --------------------------------
-- Given two lists of integers, calculate the sum of the elements 
-- at odd indices in the first list and the sum of the elements 
-- at even indices in the second list. Return these sums as a tuple.
-- Example: For the lists [1, 2, 3, 4] and [5, 6, 7, 8], 
-- the sum of the elements at odd indices in the first list is 2 + 4 = 6, and 
-- the sum of the elements at even indices in the second list is 5 + 7 = 12. 
-- The function should return the tuple (6, 12).
indexSums :: [Int] -> [Int] -> (Int,Int)
indexSums lst1 lst2 = (sum [x | (i, x) <- zip [0..] lst1, odd i], sum [x | (i, x) <- zip [0..] lst2, even i])

-- 7. --------------------------------
-- Write a function that, given a list of integers, returns 
-- all pairs of numbers (a, b) from the list where a is divisible by b.
-- Each pair should be unique, meaning that (a, b) is considered the 
-- same as (b, a) and should only appear once.
-- The order of the output does not matter if it is correct.

--divisiblePairs :: [Int] -> [(Int, Int)]

isDivisible :: Int -> Int -> Bool
isDivisible a b = b /= 0 && a `mod` b == 0

generateUniquePairs' :: [Int] -> [(Int, Int)]
generateUniquePairs' xs = 
    [(a, b) | (i, a) <- zip [0..] xs, (j, b) <- zip [0..] xs, i < j]


filterDivisiblePairs :: [(Int, Int)] -> [(Int, Int)]
filterDivisiblePairs [] = []
filterDivisiblePairs ((a, b):pairs)
    | isDivisible a b = (a, b) : filterDivisiblePairs pairs
    | otherwise = filterDivisiblePairs pairs

divisiblePairs :: [Int] -> [(Int, Int)]
divisiblePairs xs = filterDivisiblePairs (generateUniquePairs xs)

-- 8. --------------------------------
-- Write a function that takes a list of numbers and counts 
-- how many times a negative number is followed directly by a positive number.
-- Example: 
  -- [-1, 2, -3, -4, 5, 6, -7, 8, -9]
  -- -1 is followed by 2 (1 occurrence)
  -- -3 is followed by -4 (not counted)
  -- -4 is followed by 5 (2 occurrences)
  -- -7 is followed by 8 (3 occurrences)
  -- -9 is followed by nothing (not counted)
  -- result: 3

--countNegativePositivePairs :: [Int] -> Int
-- Check if a number is negative
isNegative :: Int -> Bool
isNegative x = x < 0

-- Check if a number is positive  
isPositive :: Int -> Bool
isPositive x = x > 0

-- Check if current number is negative and next is positive
isNegativePositivePair :: Int -> Int -> Bool
isNegativePositivePair current next = 
    isNegative current && isPositive next

-- Count negative-positive pairs recursively
countPairsHelper :: [Int] -> Int
countPairsHelper [] = 0
countPairsHelper [_] = 0
countPairsHelper (x:y:xs)
    | isNegativePositivePair x y = 1 + countPairsHelper (y:xs)
    | otherwise = countPairsHelper (y:xs)

-- Main function
countNegativePositivePairs :: [Int] -> Int
countNegativePositivePairs = countPairsHelper

-- 9. --------------------------------
-- You are given a shopping list where each item is represented 
-- by a tuple consisting of its price (Int) and its category (String). 
-- The tax rate depends on its category.
-- Write a function canAfford that takes the shopping list and 
-- an available amount of money (Int). The function returns True 
-- if the total cost (after tax applied) of all items in the shopping list 
-- does not exceed the available amount of money. Otherwise, it should return False.
-- The tax rates are as follows. You can use getTaxRate to get the tax rate 
-- for a specific category.
-- Food: 5%, Electronics: 15%, Clothing: 10%, 
-- Other: 8% (default rate for any unspecified category)

-- Tax rates for each category !! USE THIS FUNCTION
getTaxRate :: String -> Double
getTaxRate "food" = 0.05    -- 5% tax on food
getTaxRate "electronics" = 0.15   -- 15% tax on electronics
getTaxRate "clothing" = 0.10      -- 10% tax on clothing
getTaxRate _ = 0.08               -- Default 8% tax for other categories

--canAfford :: [(Int, String)] -> Int -> Bool
-- Calculate price with tax for a single item
calculateItemPrice :: (Int, String) -> Double
calculateItemPrice (price, category) =
    fromIntegral price * (1 + getTaxRate category)

-- Sum all item prices with tax
calculateTotalCost :: [(Int, String)] -> Double
calculateTotalCost [] = 0
calculateTotalCost (item:items) = 
    calculateItemPrice item + calculateTotalCost items

-- Compare total cost with available money
canAfford :: [(Int, String)] -> Int -> Bool
canAfford shoppingList availableMoney =
    calculateTotalCost shoppingList <= fromIntegral availableMoney

-- 10. --------------------------------
-- Write a function, which generates all perfect numbers up to a given integer n.
-- A perfect number is equal to the sum of its proper divisors.
-- (e.g., 6 is perfect because 1 + 2 + 3 = 6).

--perfectNumbers :: Int -> [Int]

--main = print(perfectNumbers 30) -- [6, 28]
--main = print(perfectNumbers 500) -- [6, 28, 496]
-- Find all divisors of a number (excluding the number itself)
findDivisors :: Int -> [Int]
findDivisors n = findDivisorsHelper n 1
  where
    findDivisorsHelper :: Int -> Int -> [Int]
    findDivisorsHelper num divisor
        | divisor >= num = []
        | num `mod` divisor == 0 = divisor : findDivisorsHelper num (divisor + 1)
        | otherwise = findDivisorsHelper num (divisor + 1)

-- Alternative without where clause:
findDivisors' :: Int -> [Int]
findDivisors' n = filter (\x -> n `mod` x == 0) [1..(n-1)]

-- Sum all divisors
sumDivisors :: Int -> Int
sumDivisors n = sumList (findDivisors n)

-- Helper function to sum a list
sumList :: [Int] -> Int
sumList [] = 0
sumList (x:xs) = x + sumList xs

-- Check if a number is perfect
isPerfect :: Int -> Bool
isPerfect n = n > 0 && sumDivisors n == n

-- Generate all perfect numbers up to n
perfectNumbers :: Int -> [Int]
perfectNumbers n = filter isPerfect [1..n]



main :: IO ()
-- main = print(countOdds 123)
-- main = print (commonElements [1, 2, 3, 4, 5] [3, 4, 5, 6, 7])  -- Output: [3, 4, 5]
-- main = print (prefixes [1, 2, 3])      -- Output: [[1], [1, 2], [1, 2, 3]]
-- main = print (ssd [[1, 2], [4, 5]] [[2, 3], [5, 6]]) -- 4
-- main = print (sumPairs [(1, 2), (3, 4), (5, 6)]) -- 21
-- main = print (indexSums [1, 2, 3, 4] [5, 6, 7, 8])