{- Progress Task 1 -}

{- Your neptun code : ANX64S -}

{- 
    Create a function squareOrDouble that takes an integer and returns:
    If the given number is 0, return "Zero".
    If the given number is even, return the square of the number.
    If the given number is odd, return double the number.
    The output must be a string. 
-}

squareOrDouble :: Int -> String
squareOrDouble num 
    | num == 0  = "Zero"
    | num `mod` 2 == 0  = show (num * num)
    | num `mod` 2 == 1  = show (2 * num)


main :: IO ()
-- Test cases:
--main = print (squareOrDouble 5)    -- "10"  (5 is odd)
--main = print (squareOrDouble 8)    -- "64"  (8 is even)
--main = print (squareOrDouble 0)    -- "Zero"   (given number is 0)
--main = print (squareOrDouble (-3)) -- "-6"  (-3 is odd)
--main = print (squareOrDouble 1)    -- "2"   (1 is odd)