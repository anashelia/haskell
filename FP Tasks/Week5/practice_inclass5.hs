--1
tails :: [[Int]] -> [[Int]]
tails outter = map (\inner -> tail inner) outter

--2
ismult10 :: [Int] -> [Bool]
ismult10 [] = []
ismult10 (x : xs) 
    | x `rem` 10 == 0     = True : ismult10 xs
    | otherwise           = False : ismult10 xs

--3
lastdigits :: [Int] -> [Int]
lastdigits lst = map (\x -> x `rem` 10) lst

--4
revsub :: [[Int]] -> [[Int]]
revsub outter = map (\inner -> reverse inner) outter

--5
lasts :: [[Int]] -> [Int]
lasts outer = concatMap (\inner -> [last inner]) outer
-- lasts outter = map last outter

--6
prodf :: [Int] -> Int
prodf x = foldr (*) 1 x

prodf2 :: [Int] -> Int
prodf2 x = product x

--7
sumsqr :: Int -> Int
sumsqr n = foldr (+) 0 (map (\x -> x*x) [1..n])

--8
notempty :: [Int] -> Bool
notempty x = not (x == [])

f8 :: [[Int]] -> [[Int]]
f8 outter = [x | x <- outter, notempty x]

f82 :: [[Int]] -> [[Int]]
f82 outter = filter notempty outter

--9
f9 :: [[Int]] -> [Int]
f9 outter = map (\inner -> foldr (+) 0 inner) outter

--10
insAux :: Int -> [Int] -> [Int]
insAux n []  = [n]
insAux n (x : xs) = x : n : xs

insertAtTwo :: Int -> [[Int]] -> [[Int]]
insertAtTwo n outter = map (insAux n) outter

--11
extract3Aux :: [Int] -> [Int]
extract3Aux [] = []
extract3Aux n = [head n] ++ [n!!((length n) `div` 2)] ++ [last n]

extract3 :: [[Int]] -> [[Int]]
extract3 outter = map extract3Aux outter

--12
checkAux :: [Int] -> Int -> Bool
checkAux list number = even (length (filter ((==) number) list)) 

checkEven :: [Int] -> Bool
checkEven list = and (map (checkAux list) list)

main :: IO ()
-- main = print(tails [[1, 2, 3], [3, 4], [5, 7, 8, 9]]) -- [[2, 3], [4], [7, 8, 9]] 

-- main = print(ismult10 [1..20])
--[False,False,False,False,False,False,False,False,False,True,False,False,False,False,False,False,False,False,False,True]

-- main = print(lastdigits [1..35])
-- [1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,0,1,2,3,4,5]

-- main = print(revsub [[1,2,3],[5,6],[],[7,8,9,10]]) -- [[3,2,1],[6,5],[],[10,9,8,7]]

-- main = print(lasts [[1,2,3],[5,6],[1],[7,8,9,10]]) -- [3,6,1,10]

-- main = print(prodf [1,5,2,4]) -- 40
-- main = print(prodf2 [1,5,2,4]) -- 40

-- main = print(sumsqr 5) -- 55
-- main = print(sumsqr 0) -- 0 -- the list is [1..0] which is empty, then foldr has as result 0
-- main = print(sumsqr (-4)) -- 0 -- same here

-- main = print(f82 [[1,2,3],[],[3,4,5],[2,2],[],[],[]]) -- [[1,2,3],[3,4,5],[2,2]]

-- main = print(f9 [[1,2,3], [3,4,5], [2,2]]) -- [6,12,4]

-- main = print(insertAtTwo 10 [[1,2], [3,4,5], [6,5,9,7], [], [8]])
-- [[1,10,2],[3,10,4,5],[6,10,5,9,7],[10],[8,10]]

-- main = print(extract3 [[1..9], [2..6], [3..11], [1..10]]) -- [[1,5,9],[2,4,6],[3,7,11],[1,6,10]]

-- main = print(checkEven [1,1,2,2,2,2,3,5,3,5]) -- True
-- main = print(checkEven [1,1,2,2,1]) -- False
-- main = print(checkEven []) -- True


