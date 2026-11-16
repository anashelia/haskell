{-# LANGUAGE ParallelListComp #-}
import Data.Char

s :: String
s = "Hello"

g1 = [toUpper c | c <- s] 

g2 = [ (x, x*2, x*2) | x <- [1, 2, 3]]

g3 = [ (i, j) | i <- [1..5], j <- [1,2]]

g4 = [ (i, j) | i <- [1, 2, 3], even i| j <- [4, 5, 6], even j]

g5 = [ x | x <- s, elem x "aeiouy"]


divisible :: Int -> Int -> Bool
divisible x n = rem x n == 0

-- collect divisors of a number in a list

divisors :: Int -> [Int]
divisors x = filter (divisible x) [1..x]

-- check if a number is prime
-- i.e. equal to the list of 1 and the number itself

prime :: Int -> Bool
prime x = divisors x == [1,x]
 
-- create primes up to a given number

primes :: Int -> [Int]
primes x = filter prime [1..x]


main :: IO ()
main = print(g5)




