import Data.Bool
import Data.Int

main :: IO ()


--- ✔ deriving Show → auto printing like Multiple 1 2 3
--- ✔ Custom instance Show → custom formatting
--- ❌ Both at the same time → error (duplicate instance)

-- Key rules for instance methods
-- No type signatures inside instance blocks. Haskell already knows the types from the class.
-- Method names must match the class exactly (+, *, negate, …).
-- You can use helper functions like simplify inside method bodies.


data Days = Mon | Tue | Wed | Thu | Fri | Sat | Sun 

instance Show Days where
    show :: Days -> String
    show Mon = "M"
    show Tue = "T"
    show Wed = "W"
    show Thu = "T"
    show Fri = "f"
    show Sat = "S"
    show Sun = "s"

-- main = print(Mon)
-- M

data Exp = Num Int | Add Exp Exp | Mul Exp Exp

instance Show Exp where
    show (Num n)       = show n 
    show (Add e1 e2)   = "(" ++ show e1 ++ "+" ++ show e2 ++ ")"
    show (Mul e1 e2)   = "(" ++ show e1 ++ "*" ++ show e2 ++ ")"

exp1, exp2 :: Exp
exp1 = Add (Mul (Num 2) (Num 2)) (Num 3)
exp2 = Mul (Num 2) (Add (Num 2) (Num 3))

-- main = print (exp1, exp2) 
-- (((2*2)+3),(2*(2+3)))

data Multiple = Multiple Int Int Int

printMultiple :: Multiple -> String
printMultiple (Multiple a b c)  = show a ++ " * " ++ show b ++ " * " ++ show c ++ " "

m1, m2 :: Multiple
m1 = Multiple 1 2 3
m2 = Multiple 4 5 6

-- data Multiple = Multiple Int Int Int
--     deriving (Show)
-- main = print (Multiple 1 2 3) --Multiple 1 2 3 
------- this way, when writing deriving show, it would print and we wouldnt need to implement instance of show on our own

-- main = print $ (printMultiple m1,printMultiple m2) 
-- ("1*2*3","4*5*6")

instance Show Multiple where
    show x@(Multiple a b c) = show a ++ " * " ++ show b ++ " * " ++ show c ++ " "
-- show x@(Multiple a b c) = printMultiple x   or we can write it like this, if we predefine printMultiple
-- main = print (m1) 

------ difference between the two mains, is that the top one cant just print, you must you printMultiple.
------ thats because we werent using instance of show there.   in the second example we used show, so we can just print it normally

data Q = Q { num :: Int , denom :: Int }

simplify :: Q -> Q
simplify (Q { num , denom }) =
  Q { num = sgn * abs num `div` n , denom = abs denom `div` n }
  where
    n = gcd (abs num) (abs denom)
    sgn | num>0 && denom>0 || num<0 && denom<0 = 1
        | num<0 && denom>0 || num>0 && denom<0 = -1
        | num == 0 && denom /= 0               = 0
        | denom == 0                           = error "denominator cannot be 0"

 
instance Show Q where
    show :: Q -> String
    show n = show (num n) ++ "/" ++ show (denom n)

-- main = print(Q 4 6)
-- main = print (simplify $ Q 4 6) -- 2/3
-- main = print $ Q (-1) 2 -- -1/2
-- main = print $ Q 2 3 -- 2/3
-- main = print $ Q 2 8 -- 2/8


data Point = Point Int Int deriving (Show, Eq, Ord) 
-- main = print (Point 1 2) -- Point 1 2

{-
class Num a where
    (+)  :: a -> a -> a
    (-)  :: a -> a -> a
    (*)  :: a -> a -> a
    negate :: a -> a
    abs    :: a -> a
    signum :: a -> a
    fromInteger :: Integer -> a
Any type that is an instance of Num 
must define these operations (or rely on defaults).

Common instances of Num: Int, Integer, Float, Double
-}

instance Num Q where
  (+) :: Q -> Q -> Q
  Q a b + Q c d = simplify $ Q (a*d + c*b) (b*d)
  (*) :: Q -> Q -> Q
  Q a b * Q c d = simplify $ Q (a*c) (b*d)
  abs :: Q -> Q
  abs (Q a b)   = Q (abs a) (abs b)
  fromInteger :: Integer -> Q
  fromInteger a = Q (fromInteger a) 1
  negate :: Q -> Q
  negate (Q a b) = simplify $ Q (-a) b
  signum :: Q -> Q
  signum         = flip Q 1 . signum . num . simplify  

  {-  Could be also written like:

instance Num Q where
    (Q a b) + (Q c d) = simplify $ Q (a*d + b*c) (b*d)
    (Q a b) * (Q c d) = simplify $ Q (a*c) (b*d)
    negate (Q a b)   = Q (negate a) b
    abs (Q a b)      = Q (abs a) (abs b)
    signum (Q a b)   = Q (signum (a*b)) 1
    fromInteger n    = Q (fromInteger n) 1
-} 

-- main = print $ Q 1 3 + Q 4 3 -- 5/3
--main = print $ Q 5 3 * Q 2 3 -- 10/9
--main = print $ abs $ Q 1 3 -- 1/3
--main = print $ abs $ Q 1 (-3) -- 1/3
--main = print (fromInteger 3 :: Q) -- 3/1
--main = print $ negate (Q 1 2) -- -1/2
--main = print $ Q 1 3 - Q 2 3 -- -1/3
--main = print $ signum $ Q 1 3 -- 1/1
--main = print $ signum $ Q (-1) 3 -- -1/1
--main = print $ signum $ Q 0 3 -- 0/1


{-
class Eq a where
    (==) :: a -> a -> Bool
    (/=) :: a -> a -> Bool

Every type that wants equality comparison must implement at least (==) 
because (/=) has a default definition, and vice-versa
i.e. (/=) is negation of (==) and the other way around
-}

instance Eq Q where
    (Q a b) == (Q c d)   = a*d == b*c 

-- instance Eq Q where
--   (==) :: Q -> Q -> Bool
--   q1 == q2   = a==c && b==d
--     where 
--         Q a b = simplify q1
--         Q c d = simplify q2

-- main = print $ Q 1 3 == Q 3 9 
-- True
-- main = print $ Q 1 3 == Q 2 3 
-- False

-- You can automatically derive Eq, then 
-- no need to write own instance
-- by writing after type definition deriving (Eq)

-- data Point = Point Int Int deriving (Show, Eq, Ord) 

-- main = print (Point 1 2 == Point 1 2)   -- True
-- main = print (Point 1 2 == Point 2 3)   -- False


instance Ord Q where
  compare :: Q -> Q -> Ordering
  compare u v = compare (a * d) (c * b)
    where
      Q a b = simplify u
      Q c d = simplify v

--main = print $ Q 2 100 > Q 1 100 -- True
--main = print $ Q 2 100 > Q 1 50 -- False
--main = print $ Q 2 100 == Q 1 50 -- True


data Tree a = Node a (Tree a) (Tree a) | Leaf deriving Show

data BTree a = BNode a (BTree a) (BTree a) | BLeaf

instance Show a => Show (BTree a) where
  show BLeaf = "L"
  show (BNode a t1 t2) = "(N " ++ show a ++ " " ++ show t1 ++ " " ++ show t2 ++ ")"

-- main = print $ BNode 1 (BNode 2 BLeaf BLeaf) BLeaf -- (N 1 (N 2 L L) L)

instance Eq a => Eq (Tree a) where
  Leaf         == Leaf            = True
  Node a t1 t2 == Node a1 t3 t4 = a == a1 && t1 == t3 && t2 == t4
  _            == _               = False

-- main = print $ Node 1 (Node 2 Leaf Leaf) Leaf == Node 1 (Node 2 Leaf Leaf) Leaf 



mapTree :: (a -> b) -> Tree a -> Tree b 
mapTree _ Leaf                 = Leaf 
mapTree f (Node x left right)  = Node (f x) (mapTree f left) (mapTree f right)

-- main = print $ mapTree (+1) (Node 1 (Node 2 Leaf Leaf) Leaf)

instance Functor Tree where
  fmap = mapTree

-- main = print $ fmap (+1) (Node 1 (Node 2 Leaf Leaf) Leaf)

instance Functor BTree where
    fmap _ BLeaf = BLeaf
    fmap f (BNode x le ri) = BNode (f x) (fmap f le) (fmap f ri)

aBTree :: BTree Int 
aBTree = BNode 1 BLeaf (BNode 2 BLeaf BLeaf)

-- main = print $ fmap (*2) aBTree





























