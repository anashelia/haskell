-- Define two data types:
-- 1. School with fields for name (String) and schoolID (Int).
-- 2. Student with fields for name (String), grade (Int), and school (School).
-- Create examples:
-- - School: Name "Greenwood High", ID 301
-- - Student: Name "John", Grade 9, School is Greenwood High.
-- Hint: Use `deriving (Show)` to make your records printable.
--
-- Write a function to promote students to the next grade if their current grade is below a given threshold.
-- The threshold is passed as the first argument.

data School = School 
  { schoolName :: String,
    schoolID   :: Int
  } deriving (Show)

data Student = Student
  { studentName   :: String,
    studentGrade  :: Int,
    studentSchool :: School
  } deriving (Show)


school1 :: School
school1 = School "Greenwood High" 301

student1 :: Student
student1 = Student "John" 9 school1

promoteStudents :: Int -> [Student] -> [Student]
promoteStudents threshold = map (\s -> if studentGrade s < threshold then s { studentGrade = studentGrade s + 1 } else s)


main :: IO ()
-- main = print $ promoteStudents 10 [Student "John" 9 (School "Greenwood High" 301), Student "Jane" 10 (School "Hilltop High" 302)] -- [Student {studentName = "John", studentGrade = 10, studentSchool = School {schoolName = "Greenwood High", schoolID = 301}},Student {studentName = "Jane", studentGrade = 10, studentSchool = School {schoolName = "Hilltop High", schoolID = 302}}]
-- main = print $ promoteStudents 9 [Student "John" 8 (School "Greenwood High" 301)] -- [Student {studentName = "John", studentGrade = 9, studentSchool = School {schoolName = "Greenwood High", schoolID = 301}}]
-- main = print $ promoteStudents 8 [] -- []