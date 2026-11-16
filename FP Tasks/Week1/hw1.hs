--1
granma :: Double -> Double -> Double -> Double
granma a o p = a * 500.0 + o * 800.0 + p * 150.5

--2
eq :: Int -> Int
eq x = x * x + 2 * x + 1

--3
quad :: Double -> Double -> Double -> Double -> Double
quad a b c x = a * x^2 + b * x + c

--4
price :: Double -> Double -> Double
price x y = x * (1.1 ** y)

main :: IO ()
-- main = print(granma 5.0 7.0 10.0)  -- 9605.0
-- main = print(eq 100) -- 10201s
-- main = print(quad 1.0 2.0 1.0 3.0)   -- 16
-- main = print(price 200.0 6.0)  -- 354.3122000000002




