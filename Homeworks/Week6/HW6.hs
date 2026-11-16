--1
processPeople :: [(Int, Int)] -> [(Int, Int)]
processPeople lst = map ( \(age, euros) -> (age, round (fromIntegral euros * 1.2)) ) (filter (\(age, euros) -> age >= 25 && euros >= 30000) lst)


--2
toDiffList :: [(Int, Int)] -> [Int]
toDiffList list = map (\x -> fst x - snd x) list

aux :: [Int] -> Int -> Int -> Int -> Int
aux [] index maxIndex maxDiff = maxIndex
aux (x:xs) index maxIndex maxDiff
    | x > maxDiff = aux xs (index + 1) index x
    | otherwise = aux xs (index + 1) maxIndex maxDiff

dayWithMaxDifference :: [(Int, Int)] -> Int
dayWithMaxDifference list = aux diffs 0 (-1) 0
    where diffs = toDiffList list

--3
isPrime :: Int -> Bool
isPrime n
    | n < 2 = False
    | otherwise = length [x | x <- [1..n] , n `mod` x == 0] == 2

--4
capitalize :: String -> String
capitalize [] = []
capitalize (x:xs) = toUpper x : xs

--5
isNum :: Char -> Bool
isNum x = x >= '0' && x <='9'

consoCnt :: [Char] -> Int
consoCnt [] = 0
consoCnt (x:xs)
    | not (elem x ['a','e','i','o','u','A','E','I','O','U']) && not (isNum x) = 1+consoCnt xs
    | otherwise = consoCnt xs


vowlCnt :: [Char] -> Int
vowlCnt [] = 0
vowlCnt (x:xs)
    | elem x ['a','e','i','o','u','A','E','I','O','U'] && not (isNum x) = 1 + vowlCnt xs
    | otherwise = vowlCnt xs

hw7 :: [String] -> [(Int,Int)]
hw7 [] = []
hw7 (x:xs) = [(vowlCnt x, consoCnt x)] ++ hw7 xs






main :: IO ()
-- main = print(processPeople [(22, 25000), (30, 50000), (40, 60000), (26, 28000), (35, 32000)]) -- Output: [(30, 40000), (40, 48000), (35, 25600)]
-- main = print(dayWithMaxDifference [(30, 20), (35, 25), (40, 10), (33, 30)]) -- 2
-- main = print (isPrime 7) -- True
-- main = print (capitalizeWords "hello world") -- "Hello World"







