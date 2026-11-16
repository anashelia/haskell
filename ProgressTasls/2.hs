{- Progress Task 2-}

{- Your neptun code : ANX64S -}

{-
    Write a function that takes two integers `x` and `n` and returns the sum of the below series:
    x + x^3 + x^5 + x^7 + ... + x^(2*n-1)

    For example:
    n=3, x=2
    2 + 2^3 + 2^5 = 42
-}

sumSeries :: Int -> Int -> Int
sumSeries n x 
    | n <= 0 = 0
    | otherwise  = x ^ (2 * n - 1) + sumSeries (n - 1) x

main :: IO ()
-- Test cases:
-- main = print(sumSeries 3 2)  -- 42
-- main = print(sumSeries 3 4)  -- 1092
-- main = print(sumSeries 0 5)  -- 0
-- main = print(sumSeries 3 5)  -- 3255