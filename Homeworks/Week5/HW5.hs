import Data.Char (toUpper)

--1
isVowel :: Char -> Bool
isVowel x = elem x "aeiou"

vowelUpper :: [String] -> [String]
vowelUpper wordsList = [map toUpper word | word <- wordsList, not (null word), isVowel (head word)]

--2
transformer :: String -> Int -> (Int -> Int)
transformer word num 
    | word == "add"       = (\x -> num + x)
    | word == "multiply"  = (\x -> num * x)
    | word == "power"     = (\x -> x ^ num)
    | otherwise           = error "invalid input"


adder5 = transformer "add" 5
multiplier3 = transformer "multiply" 3
power2 = transformer "power" 2


--3
logicChain :: [Int -> Bool] -> String -> (Int -> Bool)
logicChain functions "and" = (\x -> foldl (\res func -> (func x) && res) True functions)
logicChain functions "or" = (\x -> foldl (\res func -> (func x) || res) False functions)
logicChain functions _ = error "logic string invalid"
    
isPositive x = x > 0
isLessThan10 x = x < 10

andChain = logicChain [even, isPositive, isLessThan10] "and"
orChain = logicChain [even, isPositive] "or"

--4
transpose :: [[Int]] -> [[Int]]
transpose [] = []
transpose ([]:_) = []
transpose matrix = (map head matrix) : transpose (map tail matrix)

transposeAndSum :: [[Int]] -> (Int -> Bool) -> [[Int]]
transposeAndSum matrix func = (transpose matrix) ++ [map (\x -> sum (filter func x)) matrix]

isPos x = x > 0

--5
maxLengthWords :: [String] -> [String]
maxLengthWords [] = []
maxLengthWords xs = [w | w <- xs, length w == maxLen]
  where
    maxLen = maximum (map length xs)

main :: IO ()
-- main = print(vowelUpper ["apple", "banana", "orange", "umbrella", "grape", "avocado"])
-- main = print(vowelUpper ["", "apple", "banana"]) -- "APPLE"
-- main = print(vowelUpper []) -- []

-- main = print(adder5 3) -- 8
-- main = print(multiplier3 6) -- 18
-- main = print(power2 10) -- 100

--main = print(andChain 4) -- True
--main = print(andChain 11) -- False
--main = print(orChain (-2)) -- True
--main = print(orChain (-5)) -- False

-- main = print(transposeAndSum [[1, 2, 3], [4, 5, 6], [7, 8, 9]] even) -- [[1,4,7],[2,5,8],[3,6,9],[2,10,8]]
-- main = print(transposeAndSum [[1, 2], [2, 2], [3, 3]] odd) -- [[1,2,3],[2,2,3],[1,0,6]]
-- main = print(transposeAndSum [[(-1), 2, 3], [4, 5, 6], [(-2), (-3), 4]] isPos) -- [[-1,4,-2],[2,5,-3],[3,6,4],[5,15,4]]

-- main = print(maxLengthWords ["hello", "world", "haskell", "programming"]) -- ["programming"]
-- main = print(maxLengthWords ["sunshine", "moonlight", "starlight", "night"]) -- ["moonlight","starlight"]
-- main = print(maxLengthWords ["cat", "dog", "bat"]) -- ["cat","dog","bat"]
-- main = print(maxLengthWords []) -- []















