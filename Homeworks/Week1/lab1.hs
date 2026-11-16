-- -- first steps ------------------------------------


-- -- declarint of basic types
-- x :: Int
-- x = 42;

-- f :: Float
-- f = 3.14

-- d :: Double
-- d = 2.88888

-- c :: Char 
-- c = 'a'

-- s :: String 
-- s = "Hello, Haskell!"

-- b :: Bool 
-- b = True

-- u :: ()
-- u = ()

-- -- declaration of sequences
-- list1 :: [Int]
-- list1 = [1, 2, 3, 4, 5]

-- tuple1 :: (Int, String)
-- tuple1 = (1, "Hii")

-- -- declaration of functions

-- add :: Int -> Int -> Int
-- add x y = x + y

-- subtract1 :: Int -> Int -> Int 
-- subtract1 x y = x - y 

-- multiply :: Int -> Int -> Int
-- multiply x y = x * y

-- divide :: Float -> Float -> Float
-- divide x y = x / y

-- modulus :: Int -> Int -> Int
-- modulus x y = x `mod` y

-- power :: Int -> Int -> Int
-- power x y = x ^ y

-- logicalAnd :: Bool -> Bool -> Bool
-- logicalAnd x y = x && y

-- logicalOr :: Bool -> Bool -> Bool
-- logicalOr x y = x || y

-- isEqual :: Int -> Int -> Bool
-- isEqual x y = x == y

-- isNotEqual :: Int -> Int -> Bool
-- isNotEqual x y = x /= y

-- -- main :: IO ()
-- -- main = print x
-- -- main = print f
-- -- main = print d
-- -- main = print c
-- -- main = print s
-- -- main = print b
-- -- main = print u
-- -- main = print (x, f, s)
-- -- main = do
-- --     print x
-- --     print f
-- --     print d
-- -- main = print list1
-- -- main = print tuple1
-- -- main = print (add 3 5)
-- -- main = print (subtract1 3 5)
-- -- main = print (logicalAnd True False)
-- -- main = print (isNotEqual 5 5)


-- -- lecture1 --------------------------------------------


-- absolute0 x = if (x<0) then (-x) else x

-- absolute00 :: Int -> Int
-- absolute00 x =
--     if x<0 
--         then (-x)
--     else if (x==0)
--         then 0
--     else x


-- absolute1 :: Int -> Int
-- absolute1 x 
--     | x < 0 = -x
--     | otherwise = x

-- factorial :: Int -> Int
-- factorial x 
--     | x < 0    = error "no negatives"
--     | x == 0   = 1
--     | x > 0    = x * factorial(x-1)

-- power1 :: Int -> Int -> Int
-- power1 x y
--     | y == 0     = 1
--     | y > 0      = x * power1 x (y-1)

-- li1 :: [Int]
-- li1 = [1..10]

-- li2 :: [Int]
-- li2 = 1 : [2, 3]

-- -- main :: IO()
-- -- main = print (absolute0 (-4))
-- -- main = print(absolute1 (-4))
-- -- main = print(factorial 5)
-- -- main = print(power1 2 3)
-- -- main = print li2


-- -- p1 --------------------------------------------


-- x1 :: Int
-- x1 = 77

-- doubleFunction :: Int -> Int
-- doubleFunction x = x * 2

-- half :: Double -> Double
-- half x = x / 2

-- division11 :: Int -> Int -> Int 
-- division11 x y = x `div` y

-- st3 :: String -> String -> String
-- st3 a b  = a ++ b

-- -- main :: IO ()
-- -- main = print x1
-- -- main = print (doubleFunction 2)
-- -- main = print (half 5)
-- -- main = print (division11 10 2)
-- -- main = print (st3 "Hello" " World!")

-- m1 :: Int
-- m1 = 8848
-- k2 :: Int
-- k2 = 8611
-- dist :: Int -> Int 
-- dist x = m1 - k2 + x

-- sq :: Int -> Int
-- sq x = x * x 

-- expression :: Int -> Int -> Int -> Int -> Int
-- expression a b c d = a ^ 3 + b ^ 3 + c * d

-- granpa :: Int -> Int -> Int -> Int -> Int
-- granpa original m c f = original -  (m * 300 + c * 400 + f * 2000)

-- -- main :: IO ()
-- -- main = print (dist 77)       
-- -- main = print (dist 9)    
-- -- main = print (sq 8)       
-- -- main = print (sq 0)  
-- -- main = print (expression 1 2 3 4)
-- -- main = print (expression (-1) (-2) (-3) (-4))
-- -- main = print (granpa 20000 5 10 1)
-- -- main = print (granpa 20000 10 5 2)
