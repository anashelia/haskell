--1
remElemAt :: Int -> [Int] -> [Int]
remElemAt n lst = (take n lst) ++ (drop (n+1) lst)

--2
reorder :: [String] -> [String]
reorder (x : xs) = [last xs] ++ init xs ++ [x]

--3
initials :: [String] -> String
initials [] = ""
initials (x:xs) = [x!!0] ++ "." ++ initials xs    -- or instead of x!!0, head x

--4
minimum1 :: [Int] -> Int
minimum1 [x] = x
minimum1 (x : y : xs) 
    | x < y   = minimum1 (x : xs)
    | x > y   = minimum1 (y : xs)

--5
maximum1 :: [Int] -> Int
maximum1 [x] = x 
maximum1 (x: y : xs) 
    | x > y  = maximum1 (x : xs)
    | x < y  = maximum1 (y : xs)

maxmin :: [Int] -> String
maxmin lst = "max = " ++ show(maximum1 lst) ++ " min = " ++ show(minimum1 lst)

--6
ifOneTrue :: [Bool] -> Bool
ifOneTrue []     = False
ifOneTrue (x:xs)
    | x==True    = True
    | otherwise  = ifOneTrue xs

--7
ifAllTrue :: [Bool] -> Bool
ifAllTrue []   = True
ifAllTrue (x:xs) 
    | x==False  = False
    | otherwise = ifAllTrue xs

--8
del2 :: [Int] -> [Int]
del2 []  = []
del2 [x] = [x]
del2 (x : y : xs) = x : del2 xs

--9
fe :: [Int] -> Int
fe []  = 0
fe [x] = 0
fe (x : y : xs) 
    | x==y      = 1 + fe(y:xs)
    | otherwise = fe(y:xs)


main :: IO ()
-- main = print (remElemAt 6 [1..7]) -- [1,2,3,4,5,6]
-- main = print (remElemAt 2 [1..7]) -- [1,2,4,5,6,7]
-- main = print (remElemAt 9 [1..7]) -- [1,2,3,4,5,6,7]

-- main = print (reorder ["tail", "body", "head"])              -- ["head", "body", "tail"]
-- main = print (reorder ["tails", "body", "heads"] )           -- ["heads", "body", "tails"]
-- main = print (reorder ["ground", "rainbow", "sky"])          -- ["sky", "rainbow", "ground"]

-- main = print (initials ["Sam", "Harris"])                   -- "S.H."
-- main = print (initials ["Howard", "Phillips", "Lovecraft"]) -- "H.P.L."

-- main = print (minimum1 [1..5])        -- 1
-- main = print (minimum1 [10,9,8,7,6])  -- 6
-- main = print (minimum1 [8,6,4,10,12]) -- 4

-- main = print (maxmin [4,6,2,1,9,63,-134,566]) -- "max = 566, min = -134"
-- main = print (maxmin [-52, 56, 30, 29, -54, 0, -110]) -- "max = 56, min = -110"
-- main = print (maxmin [5]) -- "max = 5, min = 5"

-- main = print (ifOneTrue [False, False, False]) -- False
-- main = print (ifOneTrue [False, False, True]) -- True

-- main = print (ifAllTrue [True, False, True]) -- False

-- main = print (del2 [1..10]) -- [1,3,5,7,9]
-- main = print (del2 [1..11]) -- [1,3,5,7,9,11]

-- main = print (fe [1,2,2,2,3,4,3,3,2,4,5,5,5,5,5,5,5,5,5]) -- 11
-- main = print (fe [1,2,2,3,4,3,3,2,4,5,5,5])
