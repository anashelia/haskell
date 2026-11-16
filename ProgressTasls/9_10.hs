-- Define a custom data type Book for a library system:
--   Title: The book's title (String)
--   Author: The book's author (String)
--   Year: The publication year (Integer)
--   Available: A boolean indicating whether the book is available for checkout (Bool)

-- Implement the following functions:
--PT9
-- 1. bookStatus : Book -> String
--    Return a string indicating if the book is available or checked out.
--PT10
-- 2. isClassic : Book -> Bool
--    Return True if the book's publication year is before 1980, otherwise False.

data Book = Book { 
    title     :: String,
    author    :: String,
    year      :: Integer,
    available :: Bool
} deriving (Show)

book1 = Book "1984" "George Orwell" 1949 True
book2 = Book "Modern JavaScript" "John Doe" 2020 False

--PT9
bookStatus :: Book -> String
bookStatus (Book t a _ avail) = t ++ " by " ++ a ++ " is " ++ status ++ "." where
    status = if avail then "available" else "checked out"

--PT10
isClassic :: Book -> Bool
isClassic (Book _ _ y _) = y < 1980

main :: IO ()
-- main = print(bookStatus book1) -- Expected: "1984 by George Orwell is available."
-- main = print(bookStatus book2) -- Expected: "Modern JavaScript by John Doe is checked out."

-- main = print(isClassic book1) -- Expected: True
-- main = print(isClassic book2) -- Expected: False