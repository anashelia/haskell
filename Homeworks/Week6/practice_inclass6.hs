{-# LANGUAGE ParallelListComp #-}

--1
hof1 :: [Int] -> Int
hof1 lst = foldl (+) 0 (filter (\x -> x `rem` 3 == 0 && x `rem` 5 == 0) lst)

--2
hof2 :: [String] -> String
hof2 lst = foldr (\x y -> x ++ " " ++ y) "" lst

--3
hof3 :: [Int] -> [Int]
hof3 lst = dropWhile (>=0) lst

--4
hof4 :: [Int]
hof4 = take 10 (iterate (+2) 0)

hof42 :: [Int]
hof42 = [x | x <- [0,2..20]]

--5
hof5 :: [Int]
hof5 = iterate (*2) 2 

--6
hof6 :: [Int] -> [Int]
hof6 lst = scanl (+) 0 lst

--7
swap :: (a, b) -> (b, a)
swap (a, b) = (b, a)

--8
fstTriple :: (a, b, c) -> a
fstTriple (a, b, c) = a

sndTriple :: (a, b, c) -> b
sndTriple (a, b, c) = b

thdTriple :: (a, b, c) -> c
thdTriple (a, b, c) = c

--9
t1 :: [Int] -> [Char] -> [(Int, Char)]
t1 nums chars = zip nums chars

--10
t2 :: [Int] -> [Int] -> [Int]
t2 [] _ = []
t2 _ [] = []
t2 (x : xs) (y : ys) = [maximum [x, y]] ++ t2 xs ys

t22 :: [Int] -> [Int] -> [Int]
t22 x y = zipWith max x y


--11
l16 :: [(Int, Int)]
l16 = [ (i, j) | i <- [0..10] | j <- [10, 9..0]]

--12
l17 :: [(Int, Int)]
l17 = [ (i, j) | i <- [0..23], j <- [0..59]]

--13
l18 :: [(Int, Int)]
l18 = [ (i, i) | i <- [1..5] ]

--14
l19 :: [(Int, Int, Int)]
l19 = [ (i, i*2, i*3) | i <- [1..5]]

--15
sumtup :: [(Int, Int)] -> (Int, Int)
sumtup x = (sum a, sum b)
    where (a, b) = unzip x

--16
increase :: [(Int, Int)]
increase = [ (i, j) | i <- [1..5] | j <- [2..6]]

--17
tripl :: [(Int, Int, Int)]
tripl = take 5 [ (i, i+1, i+2) | i <- [1,4..]]

--18
pairs :: [[Int]] -> [([Int], [Int])]
pairs [] = []
pairs [x] = [(x, [])]
pairs (x : y : xs) = (x, y) : pairs xs

--19
triplesum :: [(Int, Int)] -> [(Int, Int, Int)]
triplesum lst = map (\(a, b) ->(a, b, (a+b))) lst

--20
quadruple :: [(Int, Int, Int, Int)]
quadruple = [ (i, i^2, i^3, i^4) | i <- [1..20]]

--21
tri :: ([Int], [Int], [Int]) -> [(Int, Int, Int)]
tri (a, b ,c) = [ (i, j, k) | i <- a | j <- b | k <- c ]
--tri x = [ (a, b, c) | a <- fst3 x | b <- snd3 x | c <- thd3 x ]
--tri (a,b,c) = zip3 a b c

--22
duplic :: [Int] -> Bool
duplic []  = False
duplic [x] = False
duplic (x : y : xs) 
    | x == y    = True
    | otherwise = duplic (y : xs)

--23
duplicRem :: [Int] -> [Int]
duplicRem [] = []
duplicRem [x] = [x]
duplicRem (x : y : xs)
    | x==y      = duplicRem (y:xs)
    | otherwise = x : duplicRem (y:xs)

--24
f :: [[[Int]]] -> [[Int]]
f x = concat x

--25
l30 :: [(Int, Int)]
l30 = [(m, d) | (m, ds) <- pairs, d <- [1..ds]]
    where pairs = zip [1..12] [31,28,31,30,31,30,31,31,30,31,30,31]


main :: IO ()

--main = print(hof1 [1..20]) -- 15
-- main = print(hof1 [1..100]) -- 315

-- main = print(hof2 ["Hello", "world"]) -- "Hello world"
--main = print(hof2 ["I" , "am", "a", "programmer"]) -- "I am a programmer"

-- main = print(hof3 [10,9..(-10)]) -- [-1,-2,-3,-4,-5,-6,-7,-8,-9,-10]

-- main = print(hof4) -- [0,2,4,6,8,10,12,14,16,18]

-- main = print(take 10 hof5) -- [1,2,4,8,16,32,64,128,256,512]

-- main = print(hof6 [1..10]) -- [0,1,3,6,10,15,21,28,36,45,55]

-- main = print(swap (1, 2)) -- (2,1)

-- main = print(fstTriple (1,2,3))
-- main = print(sndTriple (1,2,3))
-- main = print(thdTriple (1,2,3))

-- main = print(t1 [1..] ['a'..'z']) -- [(1,'a'),(2,'b'),(3,'c'),...,(26,'z')]

-- main = print(t2 [1..10] [10,9..1]) -- [10,9,8,7,6,6,7,8,9,10]

-- main = print(l16)

-- main = print(l17)

-- main = print(l18)

-- main = print(l19)

-- main = print(sumtup [(1,1), (2,2), (3,3)])

-- main = print(increase)

-- main = print(tripl)

-- main = print(pairs [[1,2,3], [5,6], [7,8,9,10], [11,3], [1..5]])
-- main = print(pairs [[1,2,3], [5,6], [7,8,9,10], [11,3]])

-- main = print(triplesum [(1,2),(2,3),(3,4),(4,5),(5,6)])

-- main = print(quadruple)

-- main = print(tri ([1..10],[20..25],[35..47]))

-- main = print(duplic [1, 1]) -- True
-- main = print(duplic [2]) -- False
-- main = print(duplic [1, 2, 3, 4, 5, 6, 7, 8, 9]) -- False
-- main = print(duplic [1, 0, 5, 0, 0, 6, 7, 5, 0, 0, 0, 8, 0, 5, 0, 0, 0]) -- True
-- main = print(duplic [1, 2, 3, 4, 4]) -- True

-- main = print(duplicRem [1, 1, 0, 5, 0, 0, 6, 0, 0, 0, 7, 5, 0, 0, 0, 0, 8, 0, 5, 0, 0, 0]) 

-- main = print(f [[[1,2,3], [3,4,5]], [[1,2,3], [3,4,5], [7,8,9]]]) -- [[1,2,3],[3,4,5],[1,2,3],[3,4,5],[7,8,9]]

-- main = print(l30)


