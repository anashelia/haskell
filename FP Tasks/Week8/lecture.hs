--------- records -----------------------------

main :: IO ()

data Date = MkDate { day :: Int, month :: Int, year :: Int} deriving (Show)

today :: Date
today = MkDate 5 12 2025

tomorrow :: Date
tomorrow = MkDate 6 12 2025

yesterday :: Date
yesterday = MkDate { year = 2025, day = 4, month = 12}

getDate :: Date -> Int
getDate = day


-- main = print today
-- main = print tomorrow
-- main = print yesterday
-- main = print (day yesterday)
-- main = print (getDate tomorrow)

data Person = MkPerson {
    name :: String,
    birthdate :: Date,
    programmer :: Bool
} deriving (Show)

me :: Person = MkPerson { 
    name = "Ana", 
    birthdate = MkDate { year = 2004, month = 10, day = 10},
    programmer = True
}

eka :: Person = MkPerson {
    name = "Eka",
    birthdate = MkDate 14 12 2004,
    programmer = True
}

someone :: Person = MkPerson {
    name = " ",
    birthdate = yesterday,
    programmer = False
}
-- main = print me
-- main = print eka
-- main = print someone

getName :: Person -> String
getName = name

getName1 :: Person -> String
getName1 (MkPerson { birthdate = _, programmer = _, name = s }) = s

getName2 :: Person -> String
getName2 (MkPerson { name = s }) = s

getName3 :: Person -> String
getName3 (MkPerson { name }) = name

getName4 :: Person -> String
getName4 (MkPerson bla1 bla2 bla3) = bla1  -- imitoro person inicializaciis dros, pirveli field aris name (anu bla1)

-- main = print (getName me)
-- main = print (getName1 me)
-- main = print (getName2 eka)
-- main = print (getName3 eka)
-- main = print (getName4 someone)

changeName :: Person -> String -> Person
changeName p n = p { name = n }

changeName1 :: Person -> String -> Person
changeName1 (MkPerson nn bd pr) n = MkPerson n bd pr

-- main = print(changeName me "Z2")
-- main = print(changeName1 me "Bush")

obtainedPhD :: Person -> Person
obtainedPhD (MkPerson name bd pr) = MkPerson ("Dr " ++ name) bd pr

obtainedPhD1 :: Person -> Person
obtainedPhD1 p = p {name = "Dr " ++ name p}

-- main = print (getName $ obtainedPhD me)
-- main = print(getName $ obtainedPhD1 me)

obtainedDegree :: Person -> Person
obtainedDegree (MkPerson nm bd pr) = MkPerson nm bd True
-- obtainedDegree MkPerson {name = nm, birthdate = bd, programmer = pr} = MkPerson { name = nm, birthdate = bd, programmer = True }
-- obtainedDegree MkPerson { name, birthdate, programmer } = MkPerson { name, birthdate, programmer = True }

obtainedDegree2 :: Person -> Person
obtainedDegree2 p = p {programmer = True}

-- main = print(obtainedDegree someone)
-- main = print(obtainedDegree2 someone)


lt :: Person -> Person -> Bool
lt (MkPerson { birthdate = MkDate { year = y1, month = m1, day = d1 } })
   (MkPerson { birthdate = MkDate { year = y2, month = m2, day = d2 } })
  = (y1 < y2) || (y1 == y2 && m1 < m2) || (y1 == y2 && m1 == m2 && d1 < d2)

psort :: [Person] -> [Person]
psort [] = []
psort (x:xs) = psort [y | y <- xs, y `lt` x] ++ [x] ++ psort [y | y <- xs, y `lt` x]

oldest :: [Person] -> Person
oldest ppls = head (psort ppls)

oldestProgrammer :: [Person] -> Person
oldestProgrammer ps = oldest (filter programmer ps)
-- oldestProgrammer ps = oldest (\p -> programmer p == True) ps
-- filter (\MkPerson{programmer=pr} -> pr == True) ps
-- filter (\p -> programmer p) ps

p1, p2, p3, p4 :: Person
p1 = MkPerson { name = "B", birthdate = MkDate 2000 1 1, programmer = False }
p2 = MkPerson { name = "D", birthdate = MkDate 3000 1 1, programmer = True }
p3 = MkPerson { name = "C", birthdate = MkDate 2000 2 2, programmer = True }
p4 = MkPerson { name = "A", birthdate = MkDate 2000 1 1, programmer = True }

ppl :: [Person]
ppl = [p1, p2, p3, p4]

-- main = print (psort ppl)
-- main = print (oldest ppl)



data Point = MkPoint { x :: Float, y :: Float, visible :: Bool} deriving (Show)

origo :: Point
origo = MkPoint { x = 0, y = 0, visible = True }
-- origo = MkPoint 0 0 True

-- main = print (origo)

data Vector = MkVector { dx :: Float, dy :: Float } deriving (Show)

distance :: Vector
distance = MkVector { dx = 2.0, dy = 3.0}
-- distance = MkVector 2.0 3.0

hide :: Point -> Point
hide p = p {visible = False}

move :: Point -> Vector -> Point
move p v = p { x = (x p) + (dx v), y = (y p) + (dy v) }  -- (x p) aris p.x anu პ-ეს იქსი

-- main = print (distance)
-- main = print (move (hide origo) distance)


data Q = MkQ { nom :: Int, dem :: Int} deriving (Show)

half :: Q 
half = MkQ 1 2 

third :: Q 
third = MkQ { nom = 1, dem = 3}

-- main = print (half, third)

gcdE :: Int -> Int -> Int
gcdE x y | x >  y = gcdE (x - y) y
         | x <  y = gcdE (y - x) x
         | x == y = x

simplpfy :: Q -> Q 
simplpfy MkQ { nom = n, dem = d}
    | d == 0     = error "denominator is 0"
    | d < 0      = MkQ { nom = (-n) `div` g, dem = d`div` g }
    | otherwise  = MkQ { nom = n `div` g, dem = d `div` g}
        where g = gcd n d

-- main = print(simplpfy (MkQ 2 6))




data Days = Mon | Tue | Wed | Thu | Fri | Sat | Sun deriving (Show, Eq)

tomorrow1 :: Days 
tomorrow1 = Mon

-- main = print tomorrow1

listOfDays :: [Days]
listOfDays = [Mon, Tue, Wed, Thu, Fri, Sat, Sun]

isWorkingDay :: Days -> Bool
isWorkingDay Sat = False
isWorkingDay Sun = False
isWorkingDay _   = True

-- main = print (isWorkingDay Mon)

nextDay :: Days -> Days
nextDay d = listOfDays !! (n+1)
    where n = head [i | (u,i) <- zip listOfDays [0..], u == d]

-- main = print(nextDay Thu)

data CheckedInt = Failure
                | Ok Int
                deriving Show

a = Failure;
b = Ok 10

-- main = print(a, b)

safeDiv :: Int -> Int -> CheckedInt
safeDiv _ 0 = Failure
safeDiv a b = Ok (a `div` b)

-- main = print (safeDiv 5 2, safeDiv 5 0)

transformToZero :: CheckedInt -> Int
transformToZero Failure  = 0
transformToZero (Ok n)   = n

-- main = print (transformToZero Failure, transformToZero (Ok 7))

data Worker = Worker String Int Days
    deriving Show

bela :: Worker
bela = Worker "Bela" 30 Fri

jozsi :: Worker
jozsi = Worker "Jozsi" 40 Mon

getWday :: Worker -> Days
getWday (Worker _ _ a) = a
-- getWday (Worker name age day) = day
-- getWday (Worker nm ag d) = d

-- main = print (getWday jozsi)

workerString :: Worker -> String
workerString p@(Worker n _ _) = "The name of the worker (" ++ show p ++ ") is " ++ n

-- main = print (workerString bela)


data IntList = Empty | Cons Int IntList

intListSum :: IntList -> Int
intListSum Empty = 0
intListSum (Cons x xs) = x + intListSum xs

-- main = print (intListSum (Cons 4 (Cons 3 (Cons 2 Empty))))

data Tree a = Node a (Tree a) (Tree a)
            | Leaf deriving (Show)

tree1 :: Tree Int
tree1 = (Node 4 (Node 2 (Node 1 Leaf Leaf) (Node 3 Leaf Leaf)) Leaf)

-- main = print tree1

size :: Tree a -> Int
size Leaf = 0
size (Node _ left right) = 1 + (size left) + (size right)

-- main = print (size tree1)

collect :: Tree a -> [a]
collect Leaf = []
collect (Node a left right) = collect left ++ [a] ++ collect right

-- main = print (collect tree1)

collectPre :: Tree a -> [a]
collectPre Leaf = []
collectPre (Node a left right) = [a] ++ collectPre left ++ collectPre right

-- main = print (collectPre tree1)

collectPost :: Tree a -> [a]
collectPost Leaf = []
collectPost (Node a left right) = collectPost left ++ collectPost right ++ [a]

-- main = print (collectPost tree1)

depth :: Tree a -> Int
depth Leaf = 0
depth (Node _ left right) = (max (depth left) (depth right)) + 1

-- main = print (depth tree1)

treesort :: [Int] -> [Int]
treesort = collectIn . toTree

toTree :: [Int] -> Tree Int
toTree [] = Leaf
toTree (x:xs) = insertT x (toTree xs)

insertT :: Int -> Tree Int -> Tree Int
insertT e Leaf = Node e Leaf Leaf
insertT e (Node x le ri)
  | e <= x    = Node x (insertT e le) ri
  | otherwise = Node x le (insertT e ri)

collectIn :: Tree Int -> [Int]
collectIn Leaf = []
collectIn (Node x le ri) = collectIn le ++ [x] ++ collectIn ri

-- main = print $ treesort [3,5,7,0]

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree funct Leaf = Leaf
mapTree funct (Node a left right) = Node (funct a) (mapTree funct left) (mapTree funct right)

-- main = print (mapTree (\x -> x * 2) tree1)
-- main = print (mapTree (\x -> fromIntegral x / 2) tree1)

foldTree :: (a -> b -> b -> b) -> b -> Tree a -> b
foldTree funct u Leaf = u
foldTree funct u (Node a left right) = funct a (foldTree funct u left) (foldTree funct u right)

-- main = print (foldTree (\a b c -> a*b*c) 1 tree1)


-- data Tree a = Node a (Tree a) (Tree a) |
--                  Leaf deriving (Show)

data Tree2 a = Node2 a (Tree2 a) (Tree2 a) | 
                 Leaf2 a 
                 deriving (Show)

data Tree3 a b = Node3 a (Tree3 a b) (Tree3 a b) |
                 Leaf3 b
                 deriving (Show)

data Tree4 a = Node4 a (Tree4 a) (Tree4 a) (Tree4 a) |
                 Leaf4
                 deriving (Show)

sumLeaves :: (Tree3 Int Float) -> Float
sumLeaves (Leaf3 ff) = ff
sumLeaves (Node3 _ left right) = sumLeaves left + sumLeaves right

tree333 :: Tree3 Int Float
tree333 = Node3 2 (Node3 1 (Leaf3 1.1) (Leaf3 2.5)) (Node3 3 (Leaf3 3.0) (Leaf3 6.9)) 

aTree4 :: Tree4 Int
aTree4 = Node4 10 (Node4 1 (Node4 7 Leaf4 Leaf4 Leaf4)
                           (Node4 8 Leaf4 Leaf4 Leaf4)
                           (Node4 9 Leaf4 Leaf4 Leaf4)) 
                  (Node4 2 Leaf4 Leaf4 Leaf4) 
                  (Node4 3 Leaf4 Leaf4 Leaf4)

-- main = print (sumLeaves tree333)
-- main = print aTree4








