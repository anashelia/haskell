--1
prodDigits :: Int -> Int
prodDigits x 
    | x < 10    = x
    | otherwise = (x `rem` 10) * (prodDigits (x `div` 10))

--2
fn :: Int -> Int
fn n 
    | n <= 11 = error "N can not be zero or negative or less then 11"
    | n == 12    = 12
    | otherwise = n  + fn(n-1) 

--3
sump :: Int -> Int
sump 0 = 0
sump n = product [1..n] + sump (n-1) 

--4
cut :: [Int] -> [[Int]]
cut x = [ take z x, drop z (take y x), take u w, drop u w]
    where y = ((length x) `div` 2)
          z = y `div` 2
          w = drop y x
          u = ((length w) `div` 2)

--5
m2 :: [Int] -> Int
m2 lst 
    | length lst == 0 = error "empty list"
    | length lst < 3  = error "length less than 3"
    | otherwise       = lst!!2

--6
f1 :: [Int] -> [Int]
f1 lst = map (\x -> x*3) lst

--7
f2 :: [Int] -> [Int]
f2 lst = map(\x -> if x < 0 then (-x) else x*x) lst

--8
isOneEven :: [Int] -> Bool
isOneEven [] = False
isOneEven (x : xs)
    | even x    = True
    | otherwise = isOneEven xs 

--9
allEven :: [Int] -> Bool
allEven [] = True
allEven (x : xs)
    | odd x     = False
    | otherwise = allEven xs 

--10
divisors :: Int -> [Int]
divisors x = [n | n <- [1..x], x `mod` n == 0]

--11
delete_3 :: [Int] -> [Int]
delete_3 [] = []
delete_3 [x] = [x]
delete_3 [x, y] = [x, y]
delete_3 (x : y : z : xs) = x : y : delete_3 xs

del3 :: [[Int]] -> [[Int]]
del3 lst = map (delete_3) lst 

--12


main :: IO ()
-- main = print(prodDigits 503) -- 0
-- main = print(prodDigits 54213) -- 120

-- main = print(fn (-10)) -- N can not be zero or negative or less then 11
-- main = print(fn 14)  -- 39

-- main = print(sump 5) -- 153

--main = print (cut [1..10]) -- [[1,2],[3,4,5],[6,7],[8,9,10]]
--main = print(cut [1..11]) -- [[1,2],[3,4,5],[6,7,8],[9,10,11]]
--main = print(cut []) --[[],[],[],[]]
--main = print(cut [21]) --[[],[],[],[21]]
--main = print(cut [1..21]) -- [[1,2,3,4,5],[6,7,8,9,10],[11,12,13,14,15],[16,17,18,19,20,21]]

-- main = print(m2 [1..5]) -- 3
-- main = print(m2 [1..4]) -- 3
-- main = print(m2 [1]) -- length less than 3 
-- main = print(m2 []) -- your list is empty

-- main = print(f1 [1,5,3,1,6])  -- [3,15,9,3,18]

-- main = print(f2 [1, 2, 0, -2, 3, -4]) -- [1,4,0,2,9,4]

-- main = print (isOneEven [1,1,3])   -- False
-- main = print (isOneEven [1..9])    -- True
-- main = print (isOneEven [2,4..14]) -- True
-- main = print (isOneEven [])        -- False

-- main = print (allEven [2,4,6])   -- True -- [2,4,6] -> even 2 && even 4 && even 6 && True
-- main = print (allEven [1..9])    -- False
-- main = print (allEven [2,4..14]) -- True
-- main = print (allEven [])        -- True

-- main = print (divisors 18) -- [1,2,3,6,9,18]
-- main = print (divisors2 18) -- [1,2,3,6,9,18]

-- main = print (delete_3 [1..15]) -- [1,2,4,5,7,8,10,11,13,14]
-- main = print (del3 [[1..5],[],[1..4],[1,5],[1],[1..3],[1..10]])
-- [[1,2,4,5],[],[1,2,4],[1,5],[1],[1,2],[1,2,4,5,7,8,10]]







