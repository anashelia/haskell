-- Define two data types:
-- 1. School with fields for name (String) and schoolID (Int).
-- 2. Student with fields for name (String), grade (Int), and school (School).
-- Create examples:
-- - School: Name "Greenwood High", ID 301
-- - Student: Name "John", Grade 9, School is Greenwood High.
-- Hint: Use `deriving (Show)` to make your records printable.
--
-- Write a function to transfer students to a new school if their grade is above a given threshold.
-- The threshold is passed as the first argument. The new school is passed as the second argument.

data School = School {
    schoolName :: String,
    schoolId :: Int
} deriving (Show)  

data Student = Student {
    studentName :: String,
    grade :: Int, 
    school :: School
} deriving (Show)

school1 :: School
school1 = School "Greenwood High" 301

student1 :: Student
student1 = Student "John" 9 school1

transferStudents :: Int -> School -> [Student] -> [Student]
transferStudents threshold newSchool students = 
    map (\student -> if grade student > threshold then student { school = newSchool } else student) students

-- main = print $ transferStudents 9 (School "Elite Academy" 999) [Student "John" 9 (School "Greenwood High" 301), Student "Jane" 10 (School "Hilltop High" 302)] -- [Student {studentName = "John", studentGrade = 9, studentSchool = School {schoolName = "Greenwood High", schoolID = 301}},Student {studentName = "Jane", studentGrade = 10, studentSchool = School {schoolName = "Elite Academy", schoolID = 999}}]
main = print $ transferStudents 10 (School "Scholars Academy" 888) [Student "Alice" 11 (School "Riverdale High" 303)] -- [Student {studentName = "Alice", studentGrade = 11, studentSchool = School {schoolName = "Scholars Academy", schoolID = 888}}]
-- main = print $ transferStudents 7 (School "New Dawn School" 777) [] -- []