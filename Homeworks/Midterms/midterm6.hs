{-1.
Write a function that takes an integer n and sum the squares 
of even numbers and subtract the sum of squares of odd numbers,
in the range of 1 to n.
Example:
sumAlternatingSquares 5  -- 10  (2^2 + 4^2 - 1^2 - 3^2 - 5^2 = 10)
sumAlternatingSquares 10 -- 220 (2^2 + 4^2 + 6^2 + 8^2 + 10^2 - 1^2 - 3^2 - 5^2 - 7^2 - 9^2 = 55)
-}
sumAlternatingSquares :: Int -> Int
sumAlternatingSquares n = sum ( map (\x -> if odd x then -(x^2) else x^2) [1..n])

{-2.
Given a positive n, generate a list of all Pythagorean triples (a,b,c) 
consisting of numbers between 1 and n (a,b,c should be less than n),
and co-prime (their greatest common divisor is 1).
A Pythagorean triple has the sum of squares of two numbers equal to 
the square of third number. Example: (4,3,5) -> (3^2 + 4^2 = 5^2)
There should be no duplicates, eg. (3,4,5) (4,3,5) and (5,3,4) are duplicates,
and a,b,c should be in increasing order.
-}
f4 :: Int -> [(Int, Int, Int)]
f4 n = [(a, b, c) | c <- [1..n-1], b <- [1..c], a <- [1..b], a*a + b*b == c*c]

{-3.
Write a function to create a string in the following way:
'S' and 's' are special character in the string. 
Whenever there are three more characters after 'S' or 's', 
replace them using this formula: "Sabc" = "acbc", see tests.
If more special letters following each other, use them to continue 
processing the string. If the number of characters after 'S' or 's' 
is less than 3, no changes applied.
-}
computeS :: String -> String
computeS [] = []
computeS ('S':a:b:c:xs) = a : c : b : c : computeS xs
computeS ('s':a:b:c:xs) = a : c : b : c : computeS xs
computeS (x:xs) = x : computeS xs


{-4.
Write a function that takes a starting number, an ending, and 
an increment number. It generates a string with numbers starting from 
the initial value, adding the increment repeatedly until the next 
number would be greater than the ending number.
Example: 
start = 2 and end = 12 and increment = 2, the output is "2 4 6 8 10 12 "
start = 1, end = 10, increment = 2, the output is "1 3 5 7 9 "
start = 10, end = 1, increment = 5 the output should be ""
-}
generateSumString :: Int -> Int -> Int -> String
generateSumString start end step = show [start, (start+step) .. end]

{-5.
Given a list of points ("pointname",(x,y)) name-coordinates and another point p,
return the point which is closest to p in a string containing the name of both. 
Print error "Empty list!" in case the list is empty.
Example:
distanceCalc [("p1", (-1, -1)), ("p2", (2, 2)), ("p3", (-2, 4))] ("c1", (2, 0))
    First measure the distance from p1 to c1, which in this case is: 3.16
    Next you do the same but for p2 to c1, which equals: 2.0
    Last you do it again but for p3 to c1, which equals: 5.65
Get the minimum of the distances and return a string which contains the 
name of the closest point and the name of the given point: 
"p2 is the closest to point c1"
Hint: 
To get the distance between two points use the following formula:
sqrt((x2-x1)^2+(y2-y1)^2)
-}

-- Calculate distance between two points
calculateDistance :: (String, (Double, Double)) -> (String, (Double, Double)) -> Double
calculateDistance (_, (x1, y1)) (_, (x2, y2)) = 
    sqrt ((x2 - x1)^2 + (y2 - y1)^2)

-- Compare two points to find which is closer to target
closerPoint :: (String, (Double, Double)) -> (String, (Double, Double)) -> (String, (Double, Double)) -> (String, (Double, Double))
closerPoint target point1 point2 =
    if calculateDistance point1 target < calculateDistance point2 target 
    then point1 
    else point2

-- Find the closest point recursively
findClosestPoint :: [((String, (Double, Double)))] -> (String, (Double, Double)) -> (String, (Double, Double))
findClosestPoint [] _ = error "Empty list in findClosestPoint"
findClosestPoint [p] _ = p
findClosestPoint (p:ps) target =
    closerPoint target p (findClosestPoint ps target)

-- Main function
distanceCalc :: [((String, (Double, Double)))] -> (String, (Double, Double)) -> String
distanceCalc [] _ = "Empty list!"
distanceCalc points target =
    let closestPoint = findClosestPoint points target
    in getName closestPoint ++ " is the closest to point " ++ getName target

-- Helper function to extract name from a point
getName :: (String, (Double, Double)) -> String
getName (name, _) = name



{-6.
Given a list of integers, return the length of the longest increasing by one 
contiguous subsequence. 
Eg. [1,2,2,3,4] Output: 3 ([2,3,4] is the longest increasing subsequence)
-}

-- Helper function to process current sequence
processSequence :: Int -> Int -> [Int] -> Int
processSequence _ currentMax [] = currentMax
processSequence count currentMax [_] = max currentMax count
processSequence count currentMax (x:y:xs)
    | y == x + 1 = processSequence (count + 1) currentMax (y:xs)
    | otherwise = processSequence 1 (max currentMax count) (y:xs)

-- Main function
t1 :: [Int] -> Int
t1 [] = 0
t1 xs = processSequence 1 0 xs



{-7.
Given a list of integers, return the product of all prime numbers found at odd indices.  
Input: [2, 3, 5, 7, 11] 
Output: 21 (Only 3 and 7 are at odd indices and prime, 3 * 7 = 21)
-}

--t3 :: [Int] -> Int
divisors :: Int -> [Int]
divisors n = filter (\x -> n `rem` x == 0) [1..n]

isPrime :: Int -> Bool
isPrime x 
    | length (divisors x) > 2   = False
    | otherwise                 = True

t33 :: [Int] -> Int
t33 lst = product [x | (x, i) <- zip lst [0..], odd i, isPrime x]


{-8.
Write a function makeNegative that takes two lists.
Make all numbers of the first given list negative.
Then eliminate these values from the second list. 
0 stays 0.
-}

makeNeg :: [Int] -> [Int]
makeNeg [] = []
makeNeg (x:xs)
    | x == 0 = 0 : makeNeg xs
    | x > 0 = (-x) : makeNeg xs
    | otherwise = x : makeNeg xs

shouldEliminate :: Int -> [Int] -> Bool
shouldEliminate _ [] = False
shouldEliminate x (y:ys)
    | x == y = True
    | otherwise = shouldEliminate x ys

-- Remove elements that are in the negative list
removeNegatives :: [Int] -> [Int] -> [Int]
removeNegatives [] _ = []
removeNegatives (x:xs) negList
    | shouldEliminate x negList = removeNegatives xs negList
    | otherwise = x : removeNegatives xs negList

-- Main function
makeNegative :: [Int] -> [Int] -> [Int]
makeNegative list1 list2 = 
    removeNegatives list2 (makeNeg list1)

{-9. 20 points
Given a list of tuples containing employees' names, ages, and salaries, 
1. What is the median salary of the employees? 5p
(If there is an even number of employees, return the average of the two middle salaries.)
2. If the employer increases the salaries by 10% for employees older than 50, 
what will be the total new salary? 5p
3. Write in a list the names of those who can not get salary increase. 5p
4. Return the list of names of employees who are younger than 30 and earn more than 250. 5p
-}

-- 1. Get median salary
getSalaries :: [(String, Int, Double)] -> [Double]
getSalaries [] = []
getSalaries ((_, _, salary):employees) = salary : getSalaries employees

sortList :: [Double] -> [Double]
sortList [] = []
sortList [x] = [x]
sortList (x:xs) = 
    sortList (filter (<= x) xs) ++ [x] ++ sortList (filter (> x) xs)

medianSalary :: [(String, Int, Double)] -> Double
medianSalary [] = 0
medianSalary employees =
    let salaries = sortList (getSalaries employees)
        n = length salaries
        mid = n `div` 2
    in if even n 
        then (salaries !! (mid - 1) + salaries !! mid) / 2
        else salaries !! mid

-- 2. Total new salary after 10% increase for employees > 50
calculateNewSalary :: (String, Int, Double) -> Double
calculateNewSalary (_, age, salary)
    | age > 50 = salary * 1.1
    | otherwise = salary

sumSalaries :: [(String, Int, Double)] -> Double
sumSalaries [] = 0
sumSalaries (e:employees) = calculateNewSalary e + sumSalaries employees

-- 3. Names of employees who cannot get salary increase
getNoIncreaseNames :: [(String, Int, Double)] -> [String]
getNoIncreaseNames [] = []
getNoIncreaseNames ((name, age, _):employees)
    | age <= 50 = name : getNoIncreaseNames employees
    | otherwise = getNoIncreaseNames employees

-- 4. Names of employees younger than 30 and earning more than 250
getYoungHighEarners :: [(String, Int, Double)] -> [String]
getYoungHighEarners [] = []
getYoungHighEarners ((name, age, salary):employees)
    | age < 30 && salary > 250 = name : getYoungHighEarners employees
    | otherwise = getYoungHighEarners employees

-- Main function to answer all questions
analyzeEmployees :: [(String, Int, Double)] -> (Double, Double, [String], [String])
analyzeEmployees employees =
    ( medianSalary employees,
      sumSalaries employees,
      getNoIncreaseNames employees,
      getYoungHighEarners employees )


main :: IO ()
-- main = print (sumAlternatingSquares 5)   -- Output: -15
-- main = print (f4 10) --[(3,4,5)]
-- main = print (generateSumString 1 10 2) -- Expected: "1 3 5 7 9 "
-- main = print (t33 [2, 3, 5, 7, 11]) -- 21