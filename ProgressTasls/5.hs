-- Ana Shelia (ANX64S) Progress Task 5
-- Remove all odd numbers from sublists that have an even number of elements. Keep the all elements of sublists that have an odd number of elements. Return the sum of each sublist.
-- You must use the higher-order functions.

removeOdds :: [[Int]] -> [Int]
removeOdds = map (\x ->
    if even (length x)
       then sum (filter even x)
       else sum x)

main :: IO ()
-- main = print (removeOdds [[1, 2], [3, 4, 5], [], [7, 8]]) -- [2,12,0,8]
-- main = print (removeOdds [[0], [1], [2]]) -- [0,1,2]
-- main = print (removeOdds [[]]) -- [0]
-- main = print (removeOdds [[5, 6, 7], [8, 9]]) -- [18,8]