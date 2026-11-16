{- Ana Shelia
   ANX64S
-}

--1
sortByStart :: [(Int, Int)] -> [(Int, Int)]
sortByStart [] = []
sortByStart (x : xs) = sortByStart [y | y<-xs, fst y < fst x ] ++ [x] ++ sortByStart [y | y <- xs, fst y >= fst x]


--2
normalizeIntervals :: [(Int, Int)] -> [(Int, Int)]
normalizeIntervals tpls = merge (sortByStart tpls)
  where
    merge [] = []
    merge [x] = [x]
    merge (x:y:xs)
      | snd x >= (fst y - 1) = merge ((fst x, max (snd x) (snd y)) : xs)
      | otherwise = x : merge (y : xs)


--3 
checkOverlap :: [(Int, Int)] -> Bool
checkOverlap [] = False
checkOverlap [_] = False
checkOverlap (x:y:xs)
  | snd x >= (fst y) = True
  | otherwise = checkOverlap (y:xs)

isOverlapping :: [(Int, Int)] -> Bool
isOverlapping lst = checkOverlap (sortByStart lst)


--4
totalCoveredLength :: [(Int, Int)] -> Int
totalCoveredLength intervals = sum (map (\(s,e) -> e - s) (normalizeIntervals intervals))




main :: IO ()
-- 1
-- main = print (sortByStart [(30,60),(0,10),(15,25),(10,15)])                  -- [(0,10),(10,15),(15,25),(30,60)]
-- main = print (sortByStart [(1,3),(3,5),(8,10)])                              -- [(1,3),(3,5),(8,10)]
-- main = print (sortByStart [(5,7),(1,2),(9,12),(3,4)])                        -- [(1,2),(3,4),(5,7),(9,12)]
-- main = print (sortByStart [])                                                -- []
-- 2
-- main = print (normalizeIntervals [(30,60),(0,10),(15,25),(10,15)])          -- [(0,25),(30,60)]
-- main = print (normalizeIntervals [(1,3),(3,5),(8,10)])                      -- [(1,5),(8,10)]
-- main = print (normalizeIntervals [(5,7),(1,2),(9,12),(3,4)])                -- [(1,7),(9,12)]
-- main = print (normalizeIntervals [])                                        -- []
-- 3
-- main = print (isOverlapping [(30,60),(0,10),(15,25),(10,15)])               -- True  (touching/overlap)
-- main = print (isOverlapping [(1,3),(3,5),(8,10)])                           -- True  (3 touches)
-- main = print (isOverlapping [(5,7),(1,2),(9,12),(3,4)])                     -- False
-- main = print (isOverlapping [])                                             -- False
-- 4
-- main = print (totalCoveredLength [(30,60),(0,10),(15,25),(10,15)])          -- (0..25)+(30..60)=25+30=55
-- main = print (totalCoveredLength [(1,3),(3,5),(8,10)])                      -- (1..5)+(8..10)=4+2=6
-- main = print (totalCoveredLength [(5,7),(1,2),(9,12),(3,4)])                -- (1..2)+(3..4)+(5..7)+(9..12)=1+1+2+3=7
-- main = print (totalCoveredLength [])                                        -- 0
