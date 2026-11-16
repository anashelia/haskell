--1
heads :: [[Int]] -> [Int]
heads lst = map (\x -> head x) lst

--2
add100 :: [Int] -> [Int]
add100 lst = map (\x -> x + 100) lst

add100' :: [Int] -> [Int]
add100' [] = []
add100' (x : xs) = (x+100) : add100' xs

--3
triples :: [Int] -> [Int]
triples lst = map (\x -> 3*x) lst

triples2 :: [Int] -> [Int]
triples2 [] = []
triples2 (x : xs) = (x*3) : triples2 xs

--4
isoddnrs :: [Int] -> [Bool]
isoddnrs lst = map odd lst

isoddnrs2 :: [Int] -> [Bool]
isoddnrs2 [] = []
isoddnrs2 (x : xs)
    | odd x     = True : isoddnrs2 xs
    | otherwise = False : isoddnrs2 xs

--5
cubes :: [Int] -> [Int]
cubes lst = map (\x -> x*x*x) lst

cubes2 :: [Int] -> [Int]
cubes2 [] = []
cubes2 (x : xs) = (x*x*x) : cubes2 xs

cubes3 :: [Int] -> [Int]
cubes3 x = map (^3) x

powersof3 :: [Int] -> [Int]
powersof3 lst = map (\x -> 3^x) lst

--6
lastdel :: [[Int]] -> [[Int]]
lastdel lst = map (\x -> if x == [] then [] else init x) lst

--7
ins0 :: [[Int]] -> [[Int]]
ins0 lst = map (\x -> 0:x) lst

ins0' :: [[Int]] -> [[Int]]
ins0' lst = map (\x -> x++[0]) lst

--8
sq :: Int -> Int
sq x = x * x 

sqrs :: [Int] -> [Int]
sqrs lst = map sq lst

--9
sqrs_lists :: [[Int]] -> [[Int]]
sqrs_lists lst = map sqrs lst

-- 8 and 9 with lambdas
sq_lambda :: [Int] -> [Int]
sq_lambda lst = map (\x -> x * x) lst

sqrs_lambda :: [[Int]] -> [[Int]]
sqrs_lambda lst = map (map (\x -> x*x)) lst

--10
rep :: Int -> Int -> [Int]
rep 0 x = []
rep n x = x : rep (n-1) x

rep1 :: Int -> Int -> [Int]
rep1 n x = replicate n x 

repList :: Int -> [Int] -> [[Int]]
repList n lst = map (rep1 n) lst

repList2 :: Int -> [Int] -> [[Int]]
repList2 n lst = map (\x -> rep n x) lst

--11
addn :: Int -> Int
addn n = foldr (+) n [1..(n-1)]

--12
f20 :: [Int] -> [Int]
f20 lst = map(\x -> x * 2) evens
    where evens = filter (\x -> x > 0)lst

f220 :: [Int] -> [Int]
f220 lst = map(\x -> x * 2) (filter (\x -> x > 0) lst)

f2220 :: [Int] -> [Int]
f2220 = map(\x -> x * 2) . filter (\x -> x > 0)

--13
f7 :: Int -> [Int] -> [Int]
f7 n = filter (\x -> x < n)

--14
f3 :: [Int] -> [Int]
f3 lst = map(\x -> x `div` 2) filtered
    where filtered = takeWhile (/= 0) lst

f33 :: [Int] -> [Int]
f33 lst = map(\x -> x `div` 2) (takeWhile ((/=) 0) lst)

--15
insSum :: [[Int]] -> [[Int]]
insSum lst = map (\sub -> sub ++ [sum sub]) lst


insLast :: [Int] -> [Int]
insLast lst = lst ++ [sum lst]

insSum2 :: [[Int]] -> [[Int]]
insSum2 lst = map insLast lst


main :: IO ()
-- main = print(heads [[1, 2, 3], [3, 4], [5, 7, 8, 9]])

-- main = print(add100 [1..8]) -- [101,102,103,104,105,106,107,108]
-- main = print(add100' [1..8]) -- [101,102,103,104,105,106,107,108]

-- main = print(triples [1..5]) -- [3,6,9,12,15]
-- main = print(triples2 [1..5]) -- [3,6,9,12,15]

-- main = print(isoddnrs [1..5])
-- main = print(isoddnrs2 [1..5]) -- [True,False,True,False,True]

-- main = print(cubes [1..10]) -- [1,8,27,64,125,216,343,512,729,1000]
-- main = print(cubes2 [1..10])
-- main = print(cubes3 [1..10])

-- main = print(powersof3 [1..10])

-- main = print(lastdel [[1,2,3],[5,6],[],[7,8,9,10]])

-- main = print(ins0 [[1,2,3],[5,6],[],[7,8,9,10]]) -- [[0,1,2,3],[0,5,6],[0],[0,7,8,9,10]]
-- main = print(ins0' [[1,2,3],[5,6],[],[7,8,9,10]]) -- [[0,1,2,3],[0,5,6],[0],[0,7,8,9,10]]

-- main = print(sqrs [1..10]) -- [1,4,9,16,25,36,49,64,81,100]
-- main = print(sqrs_lists [[1,2],[3,4,5,6],[],[7,8]]) -- [[1,4],[9,16,25,36],[],[49,64]]
-- main = print(sq_lambda [1..10])
-- main = print(sqrs_lambda [[1,2],[3,4,5,6],[],[7,8]]) -- [[1,4],[9,16,25,36],[],[49,64]]

-- main = print(rep1 3 7) -- [7,7,7]
-- main = print(repList2 3 [3..6]) -- [[3,3,3],[4,4,4],[5,5,5],[6,6,6]]

-- main = print(addn 5) -- 15
-- main = print(addn 0) -- 0
-- main = print(addn (-2))  -- 0
-- main = print(addn 10) --55

-- main = print(f2220 [1, 2, (-2), 3, (-4)]) -- [2, 4, 6]

-- main = print(f7 3 [1,5,3,2,1,6,4,3,2,1])  -- [1,2,1,2,1]

-- main = print(f33 [1, 2, (-2), 3, 0, (-4)]) -- [0, 1, -1, 1]

-- main = print(insSum2 [[1,2], [3,4,5], [6,5,9,7], [], [8]]) -- [[1,2,3],[3,4,5,12],[6,5,9,7,27],[0],[8,8]]

