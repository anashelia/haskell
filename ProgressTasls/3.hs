{- Progress Task 3 -}

{- Your neptun code : ANX64S -}

{-
	Given a 2 dimensional list of integer, remove middle element of each sublist.
	
	Note:
	[1,2,3,4,5] -> [1,2,4,5]
	[1,2,3,4,5,6] -> [1,2,4,5,6] or [1,2,3,5,6] (both are correct)
-}

middleElem :: [a] -> Int
middleElem xs = length xs `div` 2

removeSubMid :: [[Int]] -> [[Int]]
removeSubMid = map (\xs -> take (middleElem xs) xs ++ drop (middleElem xs + 1) xs)






main :: IO()
-- Test cases:
-- main = print (removeSubMid []) -- []
-- main = print (removeSubMid [[1,2,3,4,5,6,7,8], [1,2,3,4,5], [3], [3,4], []])  -- [[1,2,3,4,6,7,8],[1,2,4,5],[],[3],[]]
-- main = print (removeSubMid [[3], [5], [9, 10], []]) -- Output: [[],[],[9],[]]
