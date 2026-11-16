f1 = foldl (+) 4 [1, 2, 3]

f2 = foldl (++) "Hello " ["How ", "Are ", "You "]
f22 = foldr (++) "Hello " ["How ", "Are ", "You "]

s1 = scanr1 (+)  [1, 2, 3]
s2 = scanl (++) "Hello " ["How ", "Are ", "You "]

main :: IO ()
main = print(s1)