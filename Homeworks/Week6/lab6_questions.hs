{-# LANGUAGE ParallelListComp #-}

--1
l1 :: [Int]
-- l1 = replicate 10 13
-- l1 = take 10 (iterate id 13)
l1 = [13 | x <- [1..10]]

--2
l2 :: [(Int, Int)]
l2 = [ (i, j) | i <- [1,2], j <- [1, 2] ]

--3
l3 :: [(Int, Int)]
l3 = [ (i, j) | i <- [1..3], j <- [3, 2..1] ]

--4
l4 :: [(Int, Int)]
-- l4 = [ (i, j) | i <- [1..6] | j <- [5..10] ]
l4 = zip [1..6] [5..10]

--5
helper :: Int -> [Int]
helper x = replicate x x

l50 :: Int -> [Int]
l50 x = concatMap (helper) [1..x]

l5 :: [Int]
l5 = [ y | y <- [1..10], x <- [1..y] ]

l51 :: [Int]
l51 = [ snd (x, y) | y <- [1..10], x <- [1..y] ]

l52 :: [Int]
l52 = concat [ take x (repeat x) |  x <- [1..10] ]

--6
l6 :: [[Int]]
l6 = [ replicate y y | y <- [1..10] ]

--7
l7 :: [(Int, Int, Int)]
l7 = take 100 [ (a, b, c) | c <- [1..], b <- [1..c], a <- [1..b], a * a + b * b == c * c ]

l71 :: [(Int, Int, Int)]
l71 = [ (a, b, c) | c <- [1..100], b <- [1..c], a <- [1..b], a*a + b*b == c*c]

l72 :: [(Int, Int, Int)]
l72 =  take 100 [ (a,b,c) | a <- [1..100], b<-[1..100], c <- [1..100] , a*a + b*b == c*c && a<b && b<c ]


--8
l8 :: [Int]
l8 = [ x | x <- [1..100], even x]

l81 :: [Int]
l81 = [ x | x <- [2,4..100]]

--9
l9 :: [Int]
l9 = [ x*x | x <- [1..20], even x]

--10
l10 :: [Int]
l10 = [ 2^x| x <- [1..10]]

--11
l11 :: [Int]
l11 = [ x | x <- [1..90], 90 `rem` x == 0]

--12
l12 :: [(Int, Int)]
l12 = [ (i, j) | i <- [0..9], j <- [i..9] ]

--13
l13 :: [(Int, Char)]
l13 = [ (i, j) | i <- [1..26] | j <- ['a'..'z']]

--14
l14 :: [Bool]
l14 = [even x | x <- [1..10]]

l142 :: [Bool]
l142 = concat [ x | x <- [[False, True]], y <- [1..5]]

--15
l15 :: [Int]
l15 = [ x | x <- [1..12], 12 `rem` x == 0]

l155 :: Bool
l155 = if length l15 > 2 then False else True

l151 :: Bool
l151 = (length [ x | x <- [1..123457], mod 123457 x == 0 ] ) == 2



main :: IO ()
-- main = print(l1)     -- [13,13,13,13,13,13,13,13,13,13]
-- main = print(l2)     -- [(1,1),(1,2),(2,1),(2,2)]
-- main = print(l3)     -- [(1,3),(1,2),(1,1),(2,3),(2,2),(2,1),(3,3),(3,2),(3,1)]
-- main = print(l4)     -- [(1,5),(2,6),(3,7),(4,8),(5,9),(6,10)]
-- main = print(l5)     -- [1,2,2,3,3,3,4,4,4,4,...,10,..,10]
-- main = print(l6)     -- [[1],[2,2],[3,3,3],[4,4,4,4],...,[10,..,10]]
-- main = print(l7)
-- main = print(l71)
-- main = print(l72)
-- main = print(l81)
-- main = print(l9)
-- main = print(l10)
-- main = print(l11)
-- main = print(l12)
-- main = print(l13)
-- main = print(l14)
-- main = print(l142)
-- main = print(l155)












