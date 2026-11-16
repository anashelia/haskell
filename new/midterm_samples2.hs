{-# LANGUAGE ParallelListComp #-}
import Data.Char
import Data.List
import Data.Maybe (fromJust)

main :: IO ()
-- main = print "Run individual tests by uncommenting them below."

-------------------------------------------------------------------------------
-- 7. MIDTERM 1 (midterm1.hs)
-------------------------------------------------------------------------------

-- 7.1 Write a function that takes a non-negative integer and returns 
-- the number of odd digits in that integer.
-- If the input is negative, return an error message 
-- as a string "Negative number error".

eachDigit :: Int -> [Int]
eachDigit 0 = []
eachDigit n = eachDigit (n `div` 10) ++ [n `rem` 10]

countOdds :: Int -> Int
countOdds x
    | x < 0     = error "Negative number error"
    | otherwise = length [d | d <- eachDigit x, odd d]

-- 7.2 Write a function prefixes that takes a list and returns 
-- a list of all prefixes of the original list.

prefixes :: [a] -> [[a]]
prefixes lst = [take n lst | n <- [1..length lst]]

-- 7.3 Write a function which takes input of 2 matrices (list of lists)
-- of the same size, and output the sum of the squared differences 
-- of corresponding elements.

ssd :: [[Int]] -> [[Int]] -> Int
ssd a b = sum [ (x - y)^2 | (row1, row2) <- zip a b, (x, y) <- zip row1 row2 ]

-- 7.4 Given two lists of integers, calculate the sum of the elements 
-- at odd indices in the first list and the sum of the elements 
-- at even indices in the second list. Return these sums as a tuple.

indexSums :: [Int] -> [Int] -> (Int,Int)
indexSums lst1 lst2 = (sum [x | (i, x) <- zip [0..] lst1, odd i], sum [x | (i, x) <- zip [0..] lst2, even i])

-- 7.5 Write a function that, given a list of integers, returns 
-- all pairs of numbers (a, b) from the list where a is divisible by b.
-- Each pair should be unique, meaning that (a, b) is considered the 
-- same as (b, a) and should only appear once.

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
divisiblePairs xs = filterDivisiblePairs (generateUniquePairs' xs)

-- 7.6 Write a function that takes a list of numbers and counts 
-- how many times a negative number is followed directly by a positive number.

isNegative :: Int -> Bool
isNegative x = x < 0

isPositive :: Int -> Bool
isPositive x = x > 0

isNegativePositivePair :: Int -> Int -> Bool
isNegativePositivePair current next = isNegative current && isPositive next

countPairsHelper :: [Int] -> Int
countPairsHelper [] = 0
countPairsHelper [_] = 0
countPairsHelper (x:y:xs)
    | isNegativePositivePair x y = 1 + countPairsHelper (y:xs)
    | otherwise = countPairsHelper (y:xs)

countNegativePositivePairs :: [Int] -> Int
countNegativePositivePairs = countPairsHelper

-- 7.7 You are given a shopping list where each item is represented 
-- by a tuple consisting of its price (Int) and its category (String). 
-- The tax rate depends on its category.
-- Write a function canAfford that takes the shopping list and 
-- an available amount of money (Int).

getTaxRate :: String -> Double
getTaxRate "food" = 0.05
getTaxRate "electronics" = 0.15
getTaxRate "clothing" = 0.10
getTaxRate _ = 0.08

calculateItemPrice :: (Int, String) -> Double
calculateItemPrice (price, category) = fromIntegral price * (1 + getTaxRate category)

calculateTotalCost :: [(Int, String)] -> Double
calculateTotalCost [] = 0
calculateTotalCost (item:items) = calculateItemPrice item + calculateTotalCost items

canAfford :: [(Int, String)] -> Int -> Bool
canAfford shoppingList availableMoney = calculateTotalCost shoppingList <= fromIntegral availableMoney

-- 7.8 Write a function, which generates all perfect numbers up to a given integer n.

findDivisors' :: Int -> [Int]
findDivisors' n = filter (\x -> n `mod` x == 0) [1..(n-1)]

isPerfect :: Int -> Bool
isPerfect n = n > 0 && sum (findDivisors' n) == n

perfectNumbers :: Int -> [Int]
perfectNumbers n = filter isPerfect [1..n]

-------------------------------------------------------------------------------
-- 8. MIDTERM 2 (midterm2.hs)
-------------------------------------------------------------------------------

-- 8.1 Write a function sumOfSquaresOfDigits that takes an integer 
-- and returns the sum of the squares of its digits.
-- In case of negatives, ignore the sign.

digitsM2 :: Int -> [Int]
digitsM2 0 = []
digitsM2 n = digitsM2 (abs n `div` 10) ++ [abs n `rem` 10]

sumOfSquaresOfDigits :: Int -> Int
sumOfSquaresOfDigits n = sum [ a ^ 2 | a <- digitsM2 n ]

-- 8.2 Given a list of integers and a specific integer, 
-- calculate how many times that integer is followed 
-- in the list by an odd number, two positions away.

countOddGap :: [Int] -> Int -> Int
countOddGap [] n = 0
countOddGap [x] n = 0
countOddGap [x, y] n = 0
countOddGap (x:y:z:xs) n 
  | x == n && odd z = 1 + countOddGap (y:z:xs) n
  | otherwise = countOddGap (y:z:xs) n

-- 8.3 Write a function which takes an Integer and returns true if the number is 
-- all ones in binary representation, false otherwise.

toB :: Int -> [Int] 
toB 0 = []
toB n = toB (n `div` 2) ++ [n `rem` 2]

isAllOnes :: Int -> Bool
isAllOnes n = all (\x -> x == 1) (toB n)

-- 8.4 Write a function that takes a string and changes every vowel to O.

changeV :: Char -> Char
changeV n 
  | n `elem` "AEIOUaeiou" = 'O'
  | otherwise = n

f1_vowels :: String -> String
f1_vowels str = map changeV str

-- 8.5 Given two sorted equally long lists and a number n, 
-- create a new list which zips the elements of the two lists together.
-- Only zip if their absolute difference is equal to n. Remove duplicates.

removeD_m2 :: [(Int, Int)] -> [(Int, Int)]
removeD_m2 [] = []
removeD_m2 (x:xs) 
  | x `elem` xs = removeD_m2 xs
  | otherwise = x : removeD_m2 xs

absDiff :: [Int] -> [Int] -> Int -> [(Int, Int)]
absDiff xs ys n = removeD_m2 [ (a, b) | (a, b) <- zip xs ys, abs (a - b) == n]

-- 8.6 Write a function that takes a list of tuples, where each tuple 
-- contains a student's name and a list of their scores.
-- Return a list of tuples with the name and their average score.

average :: [Int] -> Double
average xs = fromIntegral (sum xs) / fromIntegral (length xs)  

averageScores :: [(String, [Int])] -> [(String, Double)]
averageScores xs = [ (name, average score) | (name, score) <- xs]

-- 8.7 Given a list of integers and a target number n, write a function that 
-- finds all unique pairs of integers within the list where the 
-- first element is less than n, the second element is greater than 
-- or equal to n, and the sum of the two elements is even.

findPairs :: [Int] -> Int -> [(Int, Int)] 
findPairs xs n = [ (a, b) | a <- xs, b <- xs, a < n, b >= n, even (a + b)]

-- 8.8 Write a function that takes three parameters:
-- start, end, and a Boolean value.
-- If True, return the squares of even integers.
-- If False, return the cubes of odd integers.

generateFiltered :: Int -> Int -> Bool -> [Int]
generateFiltered start end bool 
  | start > end = []
  | bool = [ a ^ 2 | a <- [start..end], even a ]
  | otherwise = [ a ^ 3 | a <- [start..end], odd a]

-- 8.9 Bird library function
birdsName :: Char -> String 
birdsName 'B' = "Bluebird"
birdsName 'C' = "Cardinal"
birdsName 'S' = "Starling"
birdsName 'K' = "Kestrel"
birdsName 'M' = "Mockingbird"
birdsName _ = error "You just discovered a new bird!"

currentLib :: [Char]
currentLib = ['B','C','S','K','M']

possibleBirds :: [String]
possibleBirds = ["Hummingbird","Dove","Dickcissel","Why Bird","Eagle","Lark"]

newGuess :: String -> [(Char, String)]
newGuess codes = [ (code, name) | code <- codes, name <- possibleBirds, code `notElem` currentLib ]

-------------------------------------------------------------------------------
-- 9. MIDTERM 3 (midterm3.hs)
-------------------------------------------------------------------------------

-- 9.1 Write a function sumOfDigitsPower that takes two integers n and p and returns 
-- True if the sum of the digits of n raised to the power p equals n.

digitsM3 :: Int -> [Int]
digitsM3 0 = []
digitsM3 n = digitsM3 (n `div` 10) ++ [n `rem` 10] 

sumOfDigitsPower :: Int -> Int -> Bool
sumOfDigitsPower n p = sum [ a ^ p | a <- digitsM3 n] == n

-- 9.2 Find the n-th value of the Jacobsthal number sequence.

jacobsthal :: Int -> Int
jacobsthal 0 = 0
jacobsthal 1 = 1
jacobsthal n = jacobsthal (n - 1) + ( 2 * jacobsthal(n - 2))

-- 9.3 Given a list of integers, remove all numbers divisible by 5.
-- For the rest, replace with difference between sum of digits and product of digits.

removeD_m3 :: [Int] -> [Int]
removeD_m3 [] = []
removeD_m3 (x:xs)
  | x `mod` 5 == 0 = removeD_m3 xs
  | otherwise = x : removeD_m3 xs

transformList :: [Int] -> [Int]
transformList xs = [ sum (digitsM3 a) - product (digitsM3 a) | a <- removeD_m3 xs ]

-- 9.4 Write a function which takes `a` as a first term, 
-- `d` as a common difference, and `n` as the number of terms 
-- and returns the arithmetic sequence.

arithmeticSequence :: Int -> Int -> Int -> [Int]
arithmeticSequence a d n = [ a + x * d | x <- [0..n-1]]

-- 9.5 Write a function which calculates the variance of a list of floats.

summ :: [Float] -> Float
summ xs = (sum xs)

square :: [Float] -> Float
square xs = sum [ a ^ 2 | a <- xs]

var :: [Float] -> Float
var xs = (square xs / fromIntegral (length xs)) - (summ xs / fromIntegral (length xs)) ^ 2

-- 9.6 Write a function that takes a list of tuples, 
-- each containing two elements of the same type,
-- and returns a single tuple that contains two lists.

mergeTuples :: [(a, b)] -> ([a], [b])
mergeTuples xs = unzip xs

-- 9.7 A prime triplet consists of three numbers that are either 
-- (p, p+2, p+6) or (p, p+4, p+6), where p is a prime number, 
-- and p+2, p+4, and p+6 are also prime numbers.

primeTriplets :: Int -> [(Int, Int, Int)]
primeTriplets n = [(p, p+2, p+6) | p <- [2..n], isPrime_M3 p, isPrime_M3 (p+2), isPrime_M3 (p+6), p <= n, p+2 <= n, p+6 <= n] ++ [(p, p+4, p+6)| p <- [2..n], isPrime_M3 p, isPrime_M3 (p+4), isPrime_M3 (p+6), p <= n, p+4 <= n, p+6 <= n]
  where isPrime_M3 x = length [ a | a <- [1..x], x `mod` a == 0] == 2

-- 9.8 Multiply each element in a sublist by the minimum of that sublist.

multiplyWithMin :: [[Int]] -> [[Int]]
multiplyWithMin xss = map (\xs -> if null xs then [] else map (\x -> x * minimum xs) xs) xss

-- 9.9 Write a function that takes a list of tuples, where each tuple contains 
-- the name of a product (a String) and its price (a Double).
-- Filter out products <= 0. Calculate total price and count valid products.

analyzePrices :: [(String, Double)] -> (Double, Int)
analyzePrices xs =
    let valid = filter (\(_, price) -> price > 0) xs
    in (sum (map snd valid), length valid)

-------------------------------------------------------------------------------
-- 10. MIDTERM 4 (midterm4.hs)
-------------------------------------------------------------------------------

-- 10.1 Given a positive integer and a digit, write a function that removes all digits 
-- in the number that are smaller than the given digit.

changeNumber :: Int -> Int -> Int
changeNumber num digit = read (filter (\c -> digitChar <= c) numStr)
  where
    numStr = show num
    digitChar = head (show digit)

-- 10.2 Write a function that takes a list and replaces
-- all the zeroes with their respective positions within that list.

zeroes2Index :: [Int] -> [Int]
zeroes2Index xs = zeroes2IndexHelper xs 0
  where
    zeroes2IndexHelper [] _ = []
    zeroes2IndexHelper (x:xs) idx
        | x == 0 = idx : zeroes2IndexHelper xs (idx + 1)
        | otherwise = x : zeroes2IndexHelper xs (idx + 1)

-- 10.3 Write a function that takes a list of tuples (Int, Int) and 
-- returns a list of tuples where each tuple contains the absolute
-- value of the difference as first element, and original tuple as second.

tupleDifferences :: [(Int, Int)] -> [(Int, (Int, Int))]
tupleDifferences xs = [(abs (a - b), (a, b)) | (a, b) <- xs]

-- 10.4 Write a function that takes a function and a list,
-- and applies the function to each element along with its index.

mapWithIndex :: (Int -> a -> b) -> [a] -> [b]
mapWithIndex f xs = mapWithIndexHelper f xs 0
  where
    mapWithIndexHelper _ [] _ = []
    mapWithIndexHelper f (x:xs) idx = f idx x : mapWithIndexHelper f xs (idx + 1)

-- 10.5 Write a function, that takes a list of tuples (name, age)
-- and returns a pair of lists: names and ages.

extractNamesAndAges :: [(String, Int)] -> ([String], [Int])
extractNamesAndAges [] = ([], [])
extractNamesAndAges ((name, age):xs) = 
    let (names, ages) = extractNamesAndAges xs
    in (name:names, age:ages)

-- 10.6 Given a string s, return another string such that 
-- even-indexed and odd-indexed characters are grouped and space-separated.

sortMyString :: String -> String
sortMyString s = evenChars ++ " " ++ oddChars
  where
    evenChars = [s !! i | i <- [0,2..length s - 1]]
    oddChars = [s !! i | i <- [1,3..length s - 1]]

-- 10.7 Write a function that takes a list of strings and returns a single string.
-- Vowels to "O", consonants to "l", space after every substring and "." at the end.

f5_chars :: [String] -> String
f5_chars [] = "."
f5_chars xs = unwords (map processWord xs) ++ " ."
  where
    processWord word = map processChar word
    processChar c
        | c `elem` "AEIOUaeiou" = 'O'
        | c `elem` "BCDFGHJKLMNPQRSTVWXYZbcdfghjklmnpqrstvwxyz" = 'l'
        | otherwise = c

-- 10.8 Given a list of lists, keep only those sublists that 
-- have more positive numbers than negative numbers. 
-- If a sublist is empty, it should be removed. 

keepMorePositives :: [[Int]] -> [[Int]]
keepMorePositives xs = filter hasMorePositives (filter (not . null) xs)
  where
    hasMorePositives sublist =
        let positives = length (filter (> 0) sublist)
            negatives = length (filter (< 0) sublist)
        in positives > negatives

-- 10.9 Bus station function.
earliestBus :: [(String, (Int, Int))] -> String -> (Int, Int)
earliestBus buses destination =
    case filter (\(dest, _) -> dest == destination) buses of
        [] -> error "No bus to the destination"
        matchingBuses -> minimumBy compareTimes (map snd matchingBuses)
  where
    compareTimes (h1, m1) (h2, m2)
        | h1 == h2 = compare m1 m2
        | otherwise = compare h1 h2

-- 10.10 Filter out invalid scores, calculate average, count above average.
analyzeScores :: [Int] -> (Double, Int)
analyzeScores scores =
    let validScores = filter (\x -> x >= 0 && x <= 100) scores
        averageScore = if null validScores then 0.0 else fromIntegral (sum validScores) / fromIntegral (length validScores)
        aboveAverage = length (filter (> averageScore) validScores)
    in (averageScore, aboveAverage)

-------------------------------------------------------------------------------
-- 11. MIDTERM 5 (midterm5.hs)
-------------------------------------------------------------------------------

-- 11.1 Consecutive grouping was covered in midterm4, kept here for completeness.
groupConsecutive :: [Int] -> [[Int]]
groupConsecutive [] = []
groupConsecutive (x:xs) =
  let n = length (takeWhile (== x) xs) + 1
      (first, rest) = splitAt n (x:xs)
  in first : groupConsecutive rest

-- 11.2 Special 'K' character processing.
computeK :: String -> String
computeK [] = []
computeK ('K':a:b:xs) = a : computeK (a:xs)
computeK ('k':a:b:xs) = a : computeK (a:xs)
computeK (x:xs) = x : computeK xs

-- 11.3 Multiplicative sequence string.
generateSumString :: Int -> Int -> Int -> String
generateSumString start end step
    | start > end = ""
    | otherwise = generateHelper start end step ""
  where
    generateHelper current end step result
        | current > end = result
        | otherwise = generateHelper (current * step) end step (result ++ show current ++ " ")

-- 11.4 Find student with the best grade average.
gradeAvg :: [(String, [Double])] -> String
gradeAvg [] = "Empty List!"
gradeAvg (x:xs) = fst (foldl compareStudents x xs)
  where
    compareStudents s1 s2
      | getAvg (snd s1) >= getAvg (snd s2) = s1
      | otherwise = s2

getAvg :: [Double] -> Double
getAvg [] = error "No grades"
getAvg grades = sum grades / fromIntegral (length grades)

-- 11.5 Employee categorization.
categorizeEmployees :: [(String, String, Int)] -> ([String], [String])
categorizeEmployees employees = 
    let highEarners = filter (\(_, _, salary) -> salary >= 3000) employees
        engineers = [name | (name, dept, _) <- highEarners, dept == "Engineering"]
        others = [name | (name, dept, _) <- highEarners, dept /= "Engineering"]
    in (engineers, others)

-- 11.6 Fibonacci sums.
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibSum :: Int -> Int
fibSum n = sum (map fib [0..n])

t2 :: [Int] -> [Int]
t2 = map fibSum

-- 11.7 Vowel, digit, consonant keeper.
keeper :: [String] -> Char -> [String]
keeper strings k
    | k `elem` "123456789" = error "Number passed in"
    | k `elem` "aeiou" = filter (elem k) strings
    | otherwise = filter (not . elem k) strings

-- 11.8 Replace elements with their occurrence count.
replaceWithOccurrence :: [Int] -> [Int]
replaceWithOccurrence xs = map countOccurrence xs
  where
    countOccurrence x = length (filter (== x) xs)

-- 11.9 Tuple checking against list occurrences.
tupleCheck :: [(Int, Int)] -> [Int] -> Bool
tupleCheck [] _ = error "Empty list!"
tupleCheck _ [] = error "Empty list!"
tupleCheck tuples nums = all checkTuple tuples
  where
    checkTuple (a, b) = isPrime_M5 b && countOccurrences a nums == b
    isPrime_M5 n = n >= 2 && null [x | x <- [2..n-1], n `mod` x == 0]
    countOccurrences x lst = length (filter (== x) lst)

-------------------------------------------------------------------------------
-- 12. MIDTERM 6 (midterm6.hs)
-------------------------------------------------------------------------------

-- 12.1 Sum squares of evens, subtract squares of odds.
sumAlternatingSquares :: Int -> Int
sumAlternatingSquares n = sum ( map (\x -> if odd x then -(x^2) else x^2) [1..n])

-- 12.2 Special 'S' character processing.
computeS :: String -> String
computeS [] = []
computeS ('S':a:b:c:xs) = a : c : b : c : computeS xs
computeS ('s':a:b:c:xs) = a : c : b : c : computeS xs
computeS (x:xs) = x : computeS xs

-- 12.3 Additive sequence string.
generateSumStringAdd :: Int -> Int -> Int -> String
generateSumStringAdd start end step = show [start, (start+step) .. end]

-- 12.4 Point distance calculator.
calculateDistance :: (String, (Double, Double)) -> (String, (Double, Double)) -> Double
calculateDistance (_, (x1, y1)) (_, (x2, y2)) = sqrt ((x2 - x1)^2 + (y2 - y1)^2)

closerPoint :: (String, (Double, Double)) -> (String, (Double, Double)) -> (String, (Double, Double)) -> (String, (Double, Double))
closerPoint target point1 point2 =
    if calculateDistance point1 target < calculateDistance point2 target 
    then point1 
    else point2

findClosestPoint :: [((String, (Double, Double)))] -> (String, (Double, Double)) -> (String, (Double, Double))
findClosestPoint [] _ = error "Empty list in findClosestPoint"
findClosestPoint [p] _ = p
findClosestPoint (p:ps) target = closerPoint target p (findClosestPoint ps target)

distanceCalc :: [((String, (Double, Double)))] -> (String, (Double, Double)) -> String
distanceCalc [] _ = "Empty list!"
distanceCalc points target =
    let closestPoint = findClosestPoint points target
    in getName closestPoint ++ " is the closest to point " ++ getName target
  where
    getName (name, _) = name

-- 12.5 Longest increasing by one contiguous subsequence.
processSequence :: Int -> Int -> [Int] -> Int
processSequence _ currentMax [] = currentMax
processSequence count currentMax [_] = max currentMax count
processSequence count currentMax (x:y:xs)
    | y == x + 1 = processSequence (count + 1) currentMax (y:xs)
    | otherwise = processSequence 1 (max currentMax count) (y:xs)

t1 :: [Int] -> Int
t1 [] = 0
t1 xs = processSequence 1 0 xs

-- 12.6 Product of primes at odd indices.
divisors :: Int -> [Int]
divisors n = filter (\x -> n `rem` x == 0) [1..n]

isPrime_M6 :: Int -> Bool
isPrime_M6 x = length (divisors x) == 2

t33 :: [Int] -> Int
t33 lst = product [x | (x, i) <- zip lst [0..], odd i, isPrime_M6 x]

-- 12.7 Make first list negative, eliminate from second.
makeNegative :: [Int] -> [Int] -> [Int]
makeNegative list1 list2 = removeNegatives list2 (makeNeg list1)
  where
    makeNeg [] = []
    makeNeg (x:xs)
        | x == 0 = 0 : makeNeg xs
        | x > 0 = (-x) : makeNeg xs
        | otherwise = x : makeNeg xs

    removeNegatives [] _ = []
    removeNegatives (x:xs) negList
        | x `elem` negList = removeNegatives xs negList
        | otherwise = x : removeNegatives xs negList

-- 12.8 Employee analysis.
analyzeEmployees :: [(String, Int, Double)] -> (Double, Double, [String], [String])
analyzeEmployees employees =
    ( medianSalary, totalNewSalary, noIncreaseNames, youngHighEarners )
  where
    getSalaries [] = []
    getSalaries ((_, _, salary):es) = salary : getSalaries es
    
    sortList [] = []
    sortList [x] = [x]
    sortList (x:xs) = sortList (filter (<= x) xs) ++ [x] ++ sortList (filter (> x) xs)

    medianSalary = 
        let salaries = sortList (getSalaries employees)
            n = length salaries
            mid = n `div` 2
        in if null employees then 0
           else if even n then (salaries !! (mid - 1) + salaries !! mid) / 2
           else salaries !! mid

    totalNewSalary = sum [if age > 50 then salary * 1.1 else salary | (_, age, salary) <- employees]

    noIncreaseNames = [name | (name, age, _) <- employees, age <= 50]

    youngHighEarners = [name | (name, age, salary) <- employees, age < 30 && salary > 250]