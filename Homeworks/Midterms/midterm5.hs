main :: IO()
main = putStrLn "Functional Programming Midterm"

{-1.
Write a function, that groups consecutive duplicate elements in a list,
return a list of lists, where each sublist contains only consecutive duplicates.
Example: [1,1,1,1,1,2,3,3,3,4,5,5] -> [[1,1,1,1,1], [2], [3,3,3], [4], [5,5]].
-}

-- groupConsecutive :: [Int] -> [[Int]]
-- groupConsecutive [] = []
-- groupConsecutive xs = groupConsecutiveHelper xs []
--   where
--     groupConsecutiveHelper [] result = reverse result
--     groupConsecutiveHelper (x:xs) [] = groupConsecutiveHelper xs [[x]]
--     groupConsecutiveHelper (x:xs) (currentGroup:rest)
--         | x == head currentGroup = groupConsecutiveHelper xs ((x:currentGroup):rest)
--         | otherwise = groupConsecutiveHelper xs ([x]:currentGroup:rest)

-- -- Alternative simpler version:
-- groupConsecutive' :: [Int] -> [[Int]]
-- groupConsecutive' [] = []
-- groupConsecutive' [x] = [[x]]
-- groupConsecutive' (x:y:xs)
--     | x == y = 
--         let (firstGroup:rest) = groupConsecutive' (y:xs)
--         in (x:firstGroup):rest
--     | otherwise = [x] : groupConsecutive' (y:xs)

groupConsecutive :: [Int] -> [[Int]]
groupConsecutive [] = []
groupConsecutive (x:xs) =
  let n = length (takeWhile (== x) xs) + 1
      (first, rest) = splitAt n (x:xs)
  in first : groupConsecutive rest



--main = print(groupConsecutive [1,1,2,3,3,3,4,5,5])  -- [[1,1], [2], [3,3,3], [4], [5,5]]
--main = print(groupConsecutive [10,10,10,20,30,30])  -- [[10,10,10], [20], [30,30]]
--main = print(groupConsecutive [5, 5, 5])            -- [[5, 5, 5]]
--main = print(groupConsecutive [1,2,3,4])            -- [[1], [2], [3], [4]]
--main = print(groupConsecutive []) --  []

------------------------------------------------

{-2.
Given a positive n, generate a list of all Pythagorean triples (a,b,c) 
consisting of numbers between 1 and n (a,b,c should be less than n).
A Pythagorean triple has the sum of squares of two numbers equal to 
the square of third number. Example: (4,3,5) -> (3^2 + 4^2 = 5^2)
There should be no duplicates, eg. (3,4,5) (4,3,5) and (5,3,4) are duplicates,
and a,b,c should be in increasing order.
-}

f3 :: Int -> [(Int,Int,Int)]
f3 n = [(a,b,c) | c <- [1..n-1], b <- [1..c-1], a <- [1..b-1], a*a + b*b == c*c]

--main = print (f3 7) -- [(3,4,5)]
--main = print (f3 30) 
-- [(3,4,5),(6,8,10),(5,12,13),(9,12,15),(8,15,17),(12,16,20),
--  (15,20,25),(7,24,25),(10,24,26),(20,21,29),(18,24,30)]
--main = print (f3 1) -- []

------------------------------------------------

{-3.
Write a function to create from a string a new one.
'K' and 'k' are special characters in the string,
whenever there are two more characters after them
replace them by the first character after 'K' or 'k',
if more special letters use them to continue processing the string.
"Kab" = "a" 
"KKa" = "K"
"KKguKabc" after step 1 "KuKabc" after step 2 "uabc"
If there is only one character or no character after 'K', 
'K' stays there.
-}

computeK :: String -> String
computeK [] = []
computeK ('K':a:b:xs) = a : computeK (a:xs)
computeK ('k':a:b:xs) = a : computeK (a:xs)
computeK (x:xs) = x : computeK xs

--main = print (computeK "Kg")  -- "Kg"
--main = print (computeK "Kfg" )  -- "f"
--main = print (computeK "Kfgksa" )  --"fs"
--main = print (computeK "sfguKfg")  -- "sfguf"
--main = print (computeK "KKguKabc" )  -- "uabc"
--main = print (computeK "KukacosKalma") -- "uacosama"

------------------------------------------------

{-4.
Write a function that takes a starting number, an ending, and 
a step number. It generates a string with numbers starting from 
the initial value, multiplying by step repeatedly until the next 
number would be greater than the ending number.
Example: 
start = 2 and end = 12 and step = 2, the output is "2 4 8 "
start = 1, end = 10, and step = 2, the output is "1 2 4 8 "
start = 10, end = 1, step = 2 output should be ""
-}

generateSumString :: Int -> Int -> Int -> String
generateSumString start end step
    | start > end = ""
    | otherwise = generateHelper start end step ""
  where
    generateHelper current end step result
        | current > end = result
        | otherwise = generateHelper (current * step) end step (result ++ show current ++ " ")

--main = print (generateSumString 2 12 2) -- "2 4 8 "
--main = print (generateSumString 1 10 2) -- "1 2 4 8 "
--main = print (generateSumString 1 15 5)  -- "1 5 "
--main = print (generateSumString 10 10 2) -- "10 "
--main = print (generateSumString 11 10 5) --  ""

------------------------------------------------

{-5.
Given a list of students with name and grades in tuples,
return the student with the best grade average. 
Print error "Empty List!" in case the list is empty, 
or a student does not have grades.
Example:
gradeAvg [("Saul", [3.2, 4.3, 4.4, 2.3]), ("Goodman", [4.7, 3.7, 4.5])])
          ("Saul", [3.2, 4.3, 4.4, 2.3]) -> "Saul" has a grade average of 3.55
          ("Goodman", [4.7, 3.7, 4.5]) -> "Goodman" has a grade average of 4.3
Out of the two averages, Goodman's average is better, so return "Goodman"
-}

-- getAvg :: [Double] -> Double
-- getAvg [] = error "No grades"
-- getAvg grades = sum grades / fromIntegral (length grades)

-- gradeAvg :: [(String, [Double])] -> String
-- gradeAvg [] = "Empty List!"
-- gradeAvg students =
--     let validStudents = filter (\(_, grades) -> not (null grades)) students
--     in if null validStudents 
--         then "Empty List!"
--         else fst (findBestStudent validStudents)
--   where
--     findBestStudent [] = error "No valid students"
--     findBestStudent [student] = student
--     findBestStudent ((name1, grades1):(name2, grades2):rest)
--         | getAvg grades1 >= getAvg grades2 = findBestStudent ((name1, grades1):rest)
--         | otherwise = findBestStudent ((name2, grades2):rest)

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

--main = print (gradeAvg [("Saul", [3.2, 4.3, 4.4, 2.3]), ("Goodman", [4.7, 3.7, 4.5])]) --"Goodman"
--main = print (gradeAvg [("Walter", [4.7, 3.7, 4.5]), ("Jesse", [1.2, 2.342, 1.1231]), ("Hank", [])]) --"Empty List!""
--main = print (gradeAvg []) --"Empty List!"

------------------------------------------------

{-6.
You have a list of employee records, each represented as a tuple (name, department, salary):
    - name is a String representing the employee's name.
    - department is a String representing the department name.
    - salary is an Int representing the employee's salary.
Write a function categorizeEmployees :: [(String, String, Int)] -> ([String], [String]) that:
    - Filters out employees who earn below 3000, regardless of their department.
    - Categorizes the remaining employees into two lists:
        The first list contains the names of employees in the "Engineering" department.
        The second list contains the names of employees in all other departments.
Return a tuple with the two lists.
-}

categorizeEmployees :: [(String, String, Int)] -> ([String], [String])
categorizeEmployees employees = 
    let highEarners = filter (\(_, _, salary) -> salary >= 3000) employees
        engineers = [name | (name, dept, _) <- highEarners, dept == "Engineering"]
        others = [name | (name, dept, _) <- highEarners, dept /= "Engineering"]
    in (engineers, others)



--main = print (categorizeEmployees [("Alice", "Engineering", 3200), ("Bob", "Marketing", 2800), ("Charlie", "Engineering", 4000), ("David", "Sales", 5000)])
-- Expected output: (["Alice", "Charlie"], ["David"])
--main = print (categorizeEmployees [("Eve", "Engineering", 2000), ("Frank", "Marketing", 2500), ("Grace", "Engineering", 1800)])
-- Expected output: ([], [])
--main = print (categorizeEmployees [("Henry", "Engineering", 3500), ("Ivy", "Engineering", 4000)])
-- Expected output: (["Henry", "Ivy"], [])
--main = print (categorizeEmployees [("Jack", "Sales", 4500), ("Kara", "Marketing", 3100)])
-- Expected output: ([], ["Jack", "Kara"])
--main = print (categorizeEmployees [("Leo", "Engineering", 3000), ("Mia", "HR", 2999), ("Nina", "Engineering", 3100), ("Oscar", "Sales", 3000)])
-- Expected output: (["Leo", "Nina"], ["Oscar"])

------------------------------------------------

{-7.
Given a list of integers, return a list where each integer is replaced with 
the sum of Fibonacci numbers up to that integer's value inclusive.   
Input: [3,4] Output: [4,7] Fibonacci sums up to 3 is 0+1+1+2=4, up to 4 is 0+1+1+2+3=7
-}

-- fib :: Int -> Int
-- fib 0 = 0
-- fib 1 = 1
-- fib n = fib (n-1) + fib (n-2)

-- fibSum :: Int -> Int
-- fibSum n = fibSumHelper n 0
--   where
--     fibSumHelper 0 sum = sum
--     fibSumHelper k sum = fibSumHelper (k-1) (sum + fib k)

-- t2 :: [Int] -> [Int]
-- t2 xs = map fibSum xs


fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibSum :: Int -> Int
fibSum n = sum (map fib [0..n])

t2 :: [Int] -> [Int]
t2 = map fibSum

--main = print (t2 [3, 4]) -- [4, 7]
--main = print (t2 [6, 8]) -- [20,54]
--main = print (t2 [5,6,7,0]) -- [12,20,33,0]
--main = print (t2 [20,30]) -- [17710,2178308]
--main = print (t2 []) -- []

------------------------------------------------

{-8.
Given a list of strings and a character k:
If k is a vowel, omit all strings that do not contain k.
If k is a digit between 1 and 9, throw an error.
If k is neither a vowel nor a digit, omit all strings containing k.
All strings and characters given are in lowercase.
-}

keeper :: [String] -> Char -> [String]
keeper strings k
    | k `elem` "123456789" = error "Number passed in"
    | k `elem` "aeiou" = filter (elem k) strings
    | otherwise = filter (not . elem k) strings

--main = print (keeper ["hello", "world", "!"] 'e') -- ["hello"]
--main = print (keeper [] 'c') -- []
--main = print (keeper ["123321", "great", "arnold", "armstrong"] 'g') --["123321", "arnold"]
--main = print (keeper ["house", "haskell", "hiking", "holiday"] '3') --"Number passed in"

------------------------------------------------

{-9.
Write a function that takes a list of integers and returns a new list
where each element is replaced with the number of times it occurs in the list.
-}

replaceWithOccurrence :: [Int] -> [Int]
replaceWithOccurrence xs = map countOccurrence xs
  where
    countOccurrence x = length (filter (== x) xs)

--main = print (replaceWithOccurrence [1, 2, 2, 3, 4, 4, 5])  -- [1,2,2,1,2,2,1]
--main = print (replaceWithOccurrence [1, 1, 1])  -- [3,3,3]
--main = print (replaceWithOccurrence [])  -- []
--main = print (replaceWithOccurrence [5, 6, 5, 6, 7, 8])  -- [2,2,2,2,1,1]

------------------------------------------------

{-10.
Given a list of tuples and a list of integers, check if every tuple in the list 
has the following property: first element of the tuple should appear in the list 
of integers exactly as many times as the second elments value. The second element 
has to be a prime number. Print an error in case the lists are empty.
Example:
tupleCheck [(6,2),(3,3),(9,2)] [6,6,3,3,3,3,9,9]
            (6,2) -> good tuple, 6 appears 2 times in our list and 2 is a prime number
                  (3,3) -> bad tuple, 3 appears 4 times and not 3 times in the list
                        (9, 2) -> good tuple, 9 appears twice in the list and 2 is a prime number
Not all tuples match our requirement => return False 
-}

isPrime :: Int -> Bool
isPrime n
    | n < 2 = False
    | otherwise = null [x | x <- [2..n-1], n `mod` x == 0]

countOccurrences :: Int -> [Int] -> Int
countOccurrences x xs = length (filter (== x) xs)

tupleCheck :: [(Int, Int)] -> [Int] -> Bool
tupleCheck [] _ = error "Empty list!"
tupleCheck _ [] = error "Empty list!"
tupleCheck tuples nums = all checkTuple tuples
  where
    checkTuple (a, b) = isPrime b && countOccurrences a nums == b


--main = print(tupleCheck [] []) --Empty list!
--main = print(tupleCheck [(2, 3), (4, 5), (1, 2)] [2,2,2,4,4,4,4,4,1,1]) --True
--main = print(tupleCheck [(6, 2), (3, 4), (9, 2)] [6,6,3,3,3,3,9,9]) --False
--main = print(tupleCheck [(6, 2), (3, 3), (9, 2)] [6,6,3,3,3,3,9,9]) --False