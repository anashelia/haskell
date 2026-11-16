-- Remove all even numbers from sublists that have an odd number of elements. Keep the all elements of sublists that have an even number of elements. Return the sum of each sublist.
-- You must use the higher-order functions.

removeEvens :: [[Int]] -> [Int]
removeEvens xss =
  concat
    [ [ sum [ x | x <- xs, even (length xs) || odd x ] ]
    | xs <- xss
    ]


main :: IO()
-- Test cases:
-- main = print (removeEvens [[1, 2], [3, 4, 5], [], [7, 8]]) -- [3,8,0,15]
-- main = print (removeEvens [[0], [1], [2]]) -- [0,1,0]
-- main = print (removeEvens [[]]) -- [0]
main = print (removeEvens [[5, 6, 7], [8, 9]]) -- [12,17]
