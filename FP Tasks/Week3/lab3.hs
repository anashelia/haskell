-- lecture1 --------------------------------------------

fDouble :: Int -> Int
fDouble x = x * 2

mapper :: [Int] -> [Int]
mapper xs = map fDouble xs

la1 = (\x -> x*2) 

la2 = (\f -> f[10, 20, 30])

la3 = map (\x -> 10 + x) [1..5]

la4 = map (+10) [1..5]

la5 = map id [1..5]

fil1 = filter even [1..10]

fil2 = filter (/= 3) [1..10]

t1 = takeWhile (/= 3) [1..10]

d1 = dropWhile (/=3) [1..10]

fi = filter even [1..10] 
tw = takeWhile even [2,4,6,7,8,9] 
dw = dropWhile even [2,4,6,7,8,9] 

la6 = filter (\x -> x `rem` 3 == 0) [2, 18, 9, 22, 17, 24, 8, 12, 27]

-- main :: IO ()
-- main = print(mapper [1..10])
-- main = print(la1 5)
-- main = print(la2 head)
-- main = print(la3)
-- main = print(la4)
-- main = print(la5)
-- main = print(fil1)
-- main = print(fil2)
-- main = print(t1)
-- main = print(d1)
-- main = print(t1, d1)
-- main = print([fi, tw, dw])
-- main = print(la6)




-- lab03_questions --------------------------------------------

first :: [Int]
first = [1, 2, 3, 4, 5]

second :: [Int]
second = [1..101]


fruits :: [String]
fruits = ["apple", "banana", "cherry"]

firstFruit :: [String] -> String
firstFruit x = head x

restFruit :: [String] -> [String]
restFruit x = tail x

transformList :: [a] -> [a]
transformList x = (tail x) ++ [head x]

concatTails :: [a] -> [a] -> [a]
concatTails x y = (tail x) ++ (tail y)

concatHeads :: [a] -> [a] -> [a]
concatHeads x y = [head x] ++ [head y]

averageList :: [Int] -> Float
averageList []    = 0
averageList x     = fromIntegral (sum x) / fromIntegral (length x)

firstLast :: [a] -> [a]
firstLast x = [head x, last x]

concatTailAndRest :: [a] -> [a]
concatTailAndRest x = tail x ++ init x

sim :: [Int] -> Bool
sim []  = True
sim [_] = True
sim x   = (head x == last x) && sim (init (tail x))

sim2 :: [Int] -> Bool
sim2 x   = x == reverse x

middle :: [Int] -> Int
middle [] = error "Empty list"
middle x = x !! ((length x )`div` 2)

cut :: [Int] -> [[Int]]
cut []  = [[],[]]
cut x = [take ((length x) `div` 2) x, drop ((length x) `div` 2) x]

cut2 :: [Int] -> [[Int]]
cut2 x = [take y x, drop y x]
    where y = (length x) `div` 2

f1 :: [Int] -> [Int]
f1 x 
    | length x == 0  = []
    | n == 0         = [(x !! 0) + 3]
    | otherwise      = [( x !! n ) + 3] ++ f1 (take n x)
    where n = (length x ) - 1 

f111 :: [Int] -> [Int]
f111 [] = []
f111 (x : xs) = (x +3) : f111 xs

f11 :: [Int] -> [Int]
f11 x = map (\x -> x + 3) x



f2helper :: [Int] -> [Int]
f2helper x = filter (\x -> x > 0) x

f2 :: [Int] -> [Int]
f2 [] = []
f2 x  = map (\x -> x*2) (f2helper x)

f22 :: [Int] -> [Int]
f22 [] = []
f22 (x : xs) 
    | x > 0     = (x*2) : f2 xs
    | otherwise = f22 xs


sq :: [Int] -> [Int]
sq [] = []
sq x = map (\x -> x^2) x

sq2 :: [Int] -> [Int]
sq2 [] = []
sq2 (x : xs) = (x^2) : sq2 xs

f4 :: [[Int]] -> [[Int]]
f4 [] = []
f4 x = map (map (\x -> x^2)) x

f44helper :: [Int] -> [Int]
f44helper []     = []
f44helper (x:xs) = (x^2) : f44helper xs

f44 :: [[Int]] -> [[Int]]
f44 [] = []
f44 (x : xs) = (f44helper x) : f44 xs

not_five :: [Int] -> [Int]
not_five [] = []
not_five x = filter (\x -> x/=5) x

not_five2 :: [Int] -> [Int]
not_five2 [] = []
not_five2 (x:xs) 
    | x == 5    = not_five2 xs
    | otherwise = x : not_five2 xs


del :: Int -> [Int] -> [Int]
del _ [] = []
del n x = filter (\x -> x /= n) x

del2 :: Int -> [Int] -> [Int]
del2 _ [] = []
del2 n (x:xs) 
    | x == n    = del2 n xs
    | otherwise = x : del2 n xs


insertZero :: [[Int]] -> [[Int]]
insertZero []   = []
insertZero [[]] = [[0]]
insertZero x    = map(\x -> 0 : x) x

insertZero2 :: [[Int]] -> [[Int]]
insertZero2 []   = []
insertZero2 [[]] = [[0]]
insertZero2 (x : xs) = [0 : x] ++ insertZero2 xs


productf :: [Int] -> Int
productf []  = 1
productf (x : xs) = x * productf xs

filterEven :: [Int] -> [Int]
filterEven []       = []
filterEven (x : xs) 
    | even x    = x : filterEven xs
    | otherwise = filterEven xs

filterEven2 :: [Int] -> [Int]
filterEven2 []       = []
filterEven2 x = filter (\x -> x `rem` 2 == 0) x  -- filter (even) x

wordLengths :: [String] -> [Int]
wordLengths []   = []
wordLengths (x : xs) = (length x) : wordLengths xs

wordLengths2 :: [String] -> [Int]
wordLengths2 x = map (\eachString -> length eachString) x

removeEnds :: [a] -> [a]
removeEnds [_] = []
removeEnds (x : xs) = init xs
-- removeEnds x = init (tail x)

reverseList :: [a] -> [a]
reverseList []   = []
reverseList (x:xs) = reverseList xs ++ [x]

reverseList2 :: [a] -> [a]
reverseList2 x = reverse x

processList :: [Int] -> [Int]
processList []  = []
processList (x:xs)
    | even x    = [3*x] ++ processList xs
    | odd x     = [2*x] ++ processList xs

processList2 :: [Int] -> [Int]
processList2 []  = []
processList2 x = map (\n -> if odd n then n*2 else n*3) x

countInList :: [Int] -> Int
countInList [] = 0
countInList (x:xs) 
    | x > 3     = 1 + countInList xs
    | otherwise = countInList xs

countGT3 :: [[Int]] -> [Int]
countGT3 [] = []
countGT3 x = map (countInList) x
-- countGT3 (x:xs) = CountInList x : countGT3 xs

elementAt :: Int -> [a] -> a
elementAt n lst 
    | n >= length lst    = error "Index out of bounts"
    | otherwise          = lst !! n

sliceList :: Int -> Int -> [a] -> [a]
sliceList start end lst 
    | start < 0          = error "violation"
    | end > length lst   = error "violation"
    | start > end        = error "violation"
    | otherwise          = drop start (take end lst) 
    --otherwise = take (end - start) (drop start lst)

firstGreaterThanFive :: [Int] -> Int
firstGreaterThanFive [] = -1
firstGreaterThanFive (x:xs) 
    | x > 5      = x 
    | otherwise  = firstGreaterThanFive xs


replaceAll :: Int -> Int -> [Int] -> [Int]
replaceAll _ _ [] = []
replaceAll x n lst = map (\y -> if y==x then n else y) lst

replaceFirst :: Int -> Int -> [Int] -> [Int]
replaceFirst _ _ [] = []
replaceFirst n newVal (x:xs) 
    | x == n       = newVal : xs
    | otherwise    = x : replaceFirst n newVal xs


replaceInts :: [Int] -> [Char]
replaceInts [] = []
replaceInts (x : xs)
    | x == 0   = 'x' : replaceInts xs
    | even x   = 'e' : replaceInts xs
    | odd x    = 'o' : replaceInts xs

replaceInts2 :: [Int] -> [Char]
replaceInts2 [] = []
replaceInts2 x = map(\num -> if num==0 then 'x' else if even num then 'e' else 'o') x


doubleElements :: [Int] -> [Int]
doubleElements [] = []
doubleElements (x:xs) 
    | even x   = x `div` 2 : doubleElements xs
    | odd x    = x * 2 : doubleElements xs
    | x == 0   = 0 : doubleElements xs

f :: Int -> Int
f 0 = 0
f n = (3 * n + 4) * (n + 5) + f (n-1)



-- main :: IO ()
-- main = print([90..99])
-- main = print([0,5..100])
-- main = print([(-9)..(-1)])
-- main = print([3..0])
-- main = print(first)
-- main = print(second)
-- main = print(firstFruit fruits)
-- main = print (firstFruit ["orange"])
-- main = print(restFruit fruits)
-- main = print (restFruit ["kiwi", "mango"])
-- main = print (restFruit ["orange"])
-- main = print (transformList ["a", "b", "c"])
-- main = print (concatTails ["a", "b", "c"] ["d", "e", "f"])
-- main = print (concatHeads [1, 2, 3] [4, 5, 6])
-- main = print (averageList [7, 14, 21, 28])
-- main = print (firstLast "hello")
-- main = print (concatTailAndRest ["a", "b", "c"])
-- main = print (sim [1, 2,  2, 1])
-- main = print (sim2 [1, 2,  2, 1])
-- main = print (middle [1..4])
-- main = print (cut [1..11])
-- main = print (cut2 [1..11])
-- main = print (f1 [1, 5, 3, 1, 6])
-- main = print (f2 [1, 2, -2, 3, -4])
-- main = print (f22 [1, 2, -2, 3, -4])
-- main = print (sq2 [1..5])
-- main = print (f44 [[1,2],[3,4,5,6],[7,8]])
-- main = print (not_five2 [5, 4, 5, 4, 3])
-- main = print (del2 5 [1, 5, 6, 7, 5, 8, 5])
-- main = print (insertZero2 [[1, 2], [3, 4], [5]])
-- main = print (productf [])
-- main = print (filterEven2 [1, 2, 3, 4, 5, 6])
-- main = print (wordLengths2 ["apple", "banana", ""])
-- main = print (removeEnds [1, 2, 3, 4])
-- main = print (reverseList ["apple", "banana", "cherry"])
-- main = print (processList2 [1, 2, 3, 4, 5])
-- main = print (countGT3 [[1, 2, 3], [4, 5, 6], [2, 3, 4]])
-- main = print(elementAt 4 "haskell")
-- main = print (sliceList 2 4 [10, 20, 30, 40, 50])
-- main = print (sliceList 3 1 [1, 2, 3, 4])
-- main = print (firstGreaterThanFive [1, 2, 6, 4, 8])
-- main = print (replaceAll 3 99 [1, 2, 3, 4, 3])
-- main = print (replaceFirst 3 99 [1, 2, 3, 4, 3])
-- main = print (replaceInts2 [0,1,2,3,4,5])
-- main = print (doubleElements [1, 2, 3])
-- main = print (f 3)





-- practice_inclass3.hs --------------------------------------------

--1
f31 :: Int -> Int -> Int
f31 0 x = 10
f31 n x = x + f31 (n-1) x 

--2
f32 :: [Int] -> [Int]
f32 x = map(\num -> if even num then (num-2) else if odd num then (num+2) else 0) x

--3
f33 :: [Int] -> [Int]
f33 (x:xs) 
    | x >= 0      = []
    | otherwise   = (x * 3) : f33 xs

--4
f34 :: [Int] -> [Int]
f34 [] = []
f34 (x:xs) 
    | x == 0      = f34 xs
    | otherwise   = (x*2) : f34 xs

--5
f35 :: Int -> Int -> [Int]
f35 base 2     = [base ^ 2]
f35 base power = f35 base (power-1) ++ [base ^ power] 

--6
f36 :: Int -> [Int] -> [[Int]]
f36 n lst  = replicate n lst

f366 :: Int -> [Int] -> [[Int]]
f366 0 _   = []
f366 n lst = lst : f366 (n-1) lst

--7
f377 :: [Int] -> [Int]
f377 lst = take (length lst `div` 2) lst ++ [0] ++ drop (length lst `div` 2) lst

f37 :: [[Int]] -> [[Int]]
f37 x = map (f377) x

--8
f38 :: [Int] -> [Int]
f38 lst = filter (\x -> x < (head lst)) lst

--9
f39 :: [[Int]] -> [[Int]]
f39 lst = filter (\inner -> (length inner) < 10) lst

--10 
f310 :: Int -> Int -> Int 
f310 x1 0   = x1
f310 x1 x2  = f310 x2 (x1 `mod` x2)


main :: IO ()
-- main = print (f31 5 2)
-- main = print (f32 [1..5])
-- main = print (f33 [-1,-3,-5,-5,2,-4,-5])
-- main = print (f34 [1,2,3,0,5,0,6,0,0,0,0])
-- main = print (f35 2 5)
-- main = print (f366 3 [1..5])
-- main = print (f37 [[1..10], [1..11], [], [1], [1,2]])
-- main = print (f382 [5,1,2,3,4,5,3,6,7,1,8])
-- main = print (f39 [[1..10], [1..11], [1..5], []])
-- main = print (f310 24 12) 





-- practice_inclass3.hs --------------------------------------------

