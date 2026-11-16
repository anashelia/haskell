--1
helper11 :: [Int] -> Int
helper11 [] = 0
helper11 (x:xs) 
    | even x    = 1 + helper11 xs
    | otherwise = helper11 xs

helper12 :: [Int] -> Int
helper12 [] = 0
helper12 (x:xs)
    | even x    = x + helper12 xs
    | otherwise = helper12 xs


countSumEvens :: [[Int]] -> [[Int]]
countSumEvens lst = map (\sublist -> [helper11 sublist, helper12 sublist]) lst

--2
helper21 :: Char -> [Char] -> Int
helper21 _ []  = 0
helper21 ch (x:xs) 
    | x==ch     = 1 + helper21 ch xs
    | otherwise = helper21 ch xs

helper22 :: Char -> [Char] -> Int
helper22 _ []  = 0
helper22 ch (x:xs) 
    | x/=ch     = 1 + helper22 ch xs
    | otherwise = helper22 ch xs

countChar :: [[Char]] -> Char -> [[Int]]
countChar lst ch = map(\sublist -> [helper21 ch sublist, helper22 ch sublist]) lst

--3
replaceAll :: Int -> Int -> [Int] -> [Int]
replaceAll old new lst = map(\number -> if number==old then new else number)lst

--4
allEven :: [Int] -> Bool
allEven []   = True
allEven (x:xs) 
    | even x   = allEven xs
    | otherwise = False

--5
consecutiveSums :: Int -> [Int] -> Bool
consecutiveSums _ [] = False
consecutiveSums _ [_] = False
consecutiveSums n (x : y : xs) 
    | x + y == n    = True
    | otherwise     = consecutiveSums n (y:xs)




main :: IO () 
-- main = print(countSumEvens [[1,2,3,4],[71,43,42,92,3,8,1,8],[90,2,4,4],[]])
-- main = print (countChar [['a','b','c','d'],['d','d','e','l'],['a','p','x'],[]] 'd')
-- main = print (replaceAll 3 9 [1, 2, 3, 4, 3, 5])
-- main = print (allEven [2, 4, 6])
-- main = print (consecutiveSums 5 [1, 2, 3, 4, 5])

