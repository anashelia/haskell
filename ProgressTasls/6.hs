{-
Generate Pairs of Strings with Same Length and Different Characters
Given two lists of strings, generate pairs (s1, s2).
s1 is from the first list and s2 is from the second list.
Both strings must have the same length.
The strings must contain different characters (i.e., no character in s1 should be present in s2).
-}


f :: [String] -> [String] -> [(String, String)]
f lst1 lst2 = [(x, y) | x <- lst1, y <- lst2, (length x) == (length y), x /= y]

main :: IO ()
-- main = print $ f ["abc", "def"] ["xyz", "mno"] -- [("abc","xyz"),("abc","mno"),("def","xyz"),("def","mno")]
-- main = print $ f ["abc", "def"] ["abc", "jkl", "mno"] -- [("abc","jkl"),("abc","mno"),("def","abc"),("def","jkl"),("def","mno")]
-- main = print $ f ["fish", "river", "candy", "door"] ["moon", "dog", "jump", "road", "craft", "shoe"] -- [("fish","moon"),("fish","jump"),("fish","road"),("door","jump")]