-- 1
lastDigit :: Int -> Int
lastDigit x 
    | x < 0     = -1
    | otherwise = x `rem` 10 

-- 2
isLeapYear :: Int -> Bool
isLeapYear x
    | (x `rem` 4 == 0) && (x `rem` 100 /= 0)   = True
    | x `rem` 400 == 0                         = True
    | otherwise                                = False

-- 3
toOctal :: Int -> Int
toOctal x
    | x < 0     = error "Negative number"
    | x == 0    = 0
    | otherwise = (toOctal(x `div` 8)) * 10 + (x `rem` 8) 

-- 4
eachDigit :: Int -> [Int]
eachDigit n 
    | n == 0     = []
    | otherwise  = eachDigit(n `div` 10) ++ [n `rem` 10]

ticketType :: Int -> String
ticketType n 
    | (even n) && (sum(eachDigit n) `rem` 2 == 1) = "VIP"
    | otherwise                                   = "Normal"



main :: IO () 
-- main = print(lastDigit 587)
-- main = print(isLeapYear 2000)
-- main = print(toOctal 1234)
-- main = print(eachDigit 1234)
-- main = print(ticketType 224388)