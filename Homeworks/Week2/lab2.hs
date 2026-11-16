-- lecture2 --------------------------------------------


-- revriting built-in functions
head1 :: [a] -> a
head1 (x : y) = x

tail1 :: [a] -> [a]
tail1 (x : y) = y

take1 :: Int -> [a] -> [a]
take1 n [] = []
take1 n (x : xs) 
    | n < 1 = []
    | otherwise = x : take(n-1) xs

drop1 :: Int -> [a] -> [a]
drop1 n [] = []
drop1 n (x : xs) 
    | n < 1 = (x : xs)
    | otherwise = drop1 (n - 1) xs

reverse1 :: [a] -> [a]
reverse1 [] = [] 
reverse1 (x : xs) = reverse1 xs ++ [x]

length1 :: [a] -> Int
length1 []  = 0
length1 [_] = 1
length1 (_ : xs) = 1 + length1 xs

last1 :: [a] -> a 
last1 [a]  = a
last1 (x : xs) = last1 xs

init1 :: [a] -> [a]
init1 [a] = []
init1 (x : xs) = x : init1 xs

elem1 :: Char -> String -> Bool
elem1 a [] = False
elem1 a (x : xs) 
    | x == a   = True
    |otherwise = elem1 a xs

concat1 :: [[a]] -> [a]
concat1 [] = []
concat1 (x : xs) = x ++ concat1 xs 

sum1 :: Int
sum1 = sum [1, 2, 3]

-- main :: IO ()
-- main = print(head1 ['a', 'b'])
-- main = print(tail1 ['a', 'b'])
-- main = print(take1 2 [1,2,3,4,5])
-- main = print(drop1 2 [1,2,3,4,5])
-- main = print(reverse1 [1,2,3,4,5])
-- main = print(length1 [1, 2, 3, 4, 5, 6])
-- main = print(last1 [1, 2, 3])
-- main = print(elem1 'a' "bc")
-- main = print(init1 [1, 2, 3])
-- main = print(concat1[[1, 2, 3], [4, 5, 6]])
-- main = print(sum1)



-- warm-up for practice

-- main :: IO ()
-- main = do 
--     print(take 3 [1..10] ++ drop 3 [1..10])
--     print ( length (concat [[1 ,2] , [3] , [4 , 5, 6, 7] , [8 , 9]]) )
--     print(elem (length[1..5]) [7..10])
--     print([1..5] ++ [0] ++ reverse [1..5])



-- more functions to practice and learn
listt :: [Int]
listt = [1, 2, 3, 4, 5]
-- listt = []

-- main :: IO ()
-- main = print(listt !! 0)
-- main = print(notElem 3 listt)
-- main = print(null listt)
-- main = print(replicate 5 "hi")
-- main = print(take 10 (repeat "abc"))
-- main = print(take 10 (repeat [1, 2, 3]))
-- main = do
--     print(take 5 (cycle [1, 2, 3]))
--     print(take 5 (repeat [1, 2, 3]))
-- main = print(show (take 3 (repeat [1, 2, 3])))

-- main = do
--     print(lines "hi\nhow\nare\nyou\n")
--     print(unlines ["hi", "how", "are you"])
--     print(words "hi my name is ana")
--     print(unwords ["hi", "how", "are you"])

splitting :: String -> [String]
splitting x = words x

addingA :: String -> [String] -> [String]
addingA _ [] = []
addingA a (x : xs) = (a ++ x) : addingA a xs

-- main :: IO ()
-- main = print(addingA "bleep" (splitting "Hi how are you"))





-- p2 --------------------------------------------

maxi :: Int -> Int -> Int
maxi x y 
    | x == y = x
    | x > y  = x
    | otherwise = y

quadruple :: Int -> Int
quadruple x = x*4

oddOrEven :: Int -> String
oddOrEven x 
    | x `rem` 2 == 0  = "Even"
    | otherwise   = "Odd"

isoddnr :: Int -> Bool
isoddnr x = x `rem` 2 == 1

mult10 :: Int -> Bool
mult10 x = x `rem` 10 == 0

isDivisible :: Int -> Int -> Bool
isDivisible x y 
    | y == 0    = error "Division by 0"
    | otherwise = x `rem` y == 0

divAny :: Int -> Int -> Bool
divAny x y = x `rem` y == 0 || y `rem` x == 0

power1 :: Int -> Int -> Int
power1 x n = x^n 

power2 :: Int -> Int -> Int
power2 x n 
    | n == 0    = 1
    | otherwise = x * power2 x (n-1)

sumpowers :: Int -> Int
sumpowers n 
    | n < 0   = error "Negative Number"
    | n == 0  = 0
    | otherwise = n^n + sumpowers (n-1)


getLastPositive :: Int -> Int
getLastPositive x 
    | x < 0     = error "Negative Number"
    | otherwise = x - (x `mod` 10)

av5 :: Int -> Int -> Int -> Int -> Int -> Double
av5 a b c d e = fromIntegral (a + b + c + d + e) / 5.0

sursolid :: Int -> Int
sursolid x = x^5

squareSum :: Int -> Int
squareSum n 
    | n < 0    = error "Negative Number"
    | n == 0   = 0
    |otherwise = n^2 + squareSum (n-1)

oddEven :: Int -> Int -> Int
oddEven x y 
    | x `rem` 2 == 0 && y `rem` 2 == 0   = x + y
    | x `rem` 2 == 1 && y `rem` 2 == 1   = x * y
    | otherwise                          = 0

isSorted :: Int -> Int -> Int -> Int -> Int -> Bool
isSorted a b c d e = (a <= b) && (b <= c) && (c <= d) && (d <= e)

transform :: Int -> String
transform x = show(x `div` 365) ++ " years " ++ show((x `mod` 365) `div` 7) ++ " weeks " ++ show((x `mod` 365) `mod` 7) ++ " days"

digitToString :: Int -> String
digitToString n 
    | n > 5 || n < 0  = error "greater than 5"
    | n == 1   = "One"
    | n == 2   = "Two"
    | n == 3   = "Three"
    | n == 4   = "Four"
    | n == 5   = "Five"

sumOdd :: Int -> Int
sumOdd x 
    | x < 0           = error "negative number"
    | x == 1          = 1
    | x `rem` 2 == 1  = x + sumOdd (x-2)
    | x `rem` 2 == 0  = sumOdd (x-1)

equation :: Int -> Int
equation n 
    | n == 0    = 0
    | otherwise = 2*n * (2*n + 1) + equation(n-1)

-- main :: IO ()
-- main = print(maxi 5 65)
-- main = print(quadruple 3)
-- main = print(oddOrEven 5)
-- main = print(isoddnr 5)
-- main = print(mult10 101)
-- main = print(isDivisible 10 5)
-- main = print(divAny 44 110)
-- main = print(power1 2 5)
-- main = print(power2 2 5)
-- main = print(sumpowers (-5))
-- main = print(getLastPositive 5856)
-- main = print(av5 1 2 3 4 5)
-- main = print( sursolid 3 ) 
-- main = print(squareSum 0)
-- main = print(oddEven 7 7)
-- main = print(isSorted 4 3 2 1 0)
-- main = print(transform 375)
-- main = print( digitToString 3)
-- main = print (sumOdd 21)
-- main = print(equation 4)





-- practice_inclass2 --------------------------------------------

-- 1
helper1 :: Int -> Int
helper1 0 = 0
helper1 x 
    | ( (x `rem` 10) `rem` 3 == 0 )  = 1 + helper1 (x `div` 10)
    | otherwise                      = helper1 (x `div` 10)

multipleOf3 :: Int -> Int -> Int
multipleOf3 x y = helper1 (x + y)

-- 2
fibb1 :: Int -> Int
fibb1 0 = 0
fibb1 1 = 1
fibb1 n = fibb1(n-1) + fibb1(n-2)

fibb2 :: Int -> [Int]
fibb2 n = filter odd (map fibb1 [1..n])

fibb3 :: Int -> Int
fibb3 n = sum (fibb2 n)

--3
eachDigit :: Int -> [Int]
eachDigit 0 = []
eachDigit n = eachDigit (n `div` 10) ++ [n `rem` 10]

sumEvenOdd :: Int -> String -> Int
sumEvenOdd n s 
    | s == "Even"   = sum (filter even (eachDigit n))
    | s == "Odd"    = sum (filter odd (eachDigit n))
    | otherwise     = error "wrong input word"



main :: IO () 
-- main = print(multipleOf3 430 561)
-- main = print(fibb3 10)
-- main = print(eachDigit 125)
-- main = print(sumEvenOdd 745209 "Odd")