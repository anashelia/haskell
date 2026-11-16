{-# LANGUAGE ParallelListComp #-}

main :: IO ()

data Team = Team {teamName :: String, teamID :: Int} deriving (Show)

data Player = Player {playerID :: Int, playerName :: String, playerTeam :: Team, playerScores :: [Int]} deriving (Show)

data Univ = Univ {uniName :: String, uniID :: Int} deriving (Show)

data Student = Student {neptunID :: Int, studentName :: String, uni :: Univ, grades :: [Int]}

team1 = Team {teamName = "Team A", teamID = 1}

team2 = Team {teamName = "Team B", teamID = 2}

team3 = Team {teamName = "Team C", teamID = 3}

team0 = Team {teamName = "Team D", teamID = 0}

player1 = Player {playerID = 1, playerName = "Alice", playerTeam = team1, playerScores = [10, 20, 30]}

player2 = Player {playerID = 2, playerName = "Bob", playerTeam = team1, playerScores = [15, 25, 35]}

player3 = Player {playerID = 3, playerName = "Charlie", playerTeam = team2, playerScores = [12, 22, 32]}

player4 = Player {playerID = 4, playerName = "David", playerTeam = team2, playerScores = [18, 28, 38]}

player5 = Player {playerID = 5, playerName = "Eve", playerTeam = team3, playerScores = [14, 24, 34]}

player6 = Player {playerID = 6, playerName = "Frank", playerTeam = team3, playerScores = [16, 26, 36]}

univ1 = Univ {uniName = "University A", uniID = 1}

univ2 = Univ {uniName = "University B", uniID = 2}

univ3 = Univ {uniName = "University C", uniID = 3}

student1 = Student {neptunID = 101, studentName = "Alice", uni = univ1, grades = [85, 90, 78]}

student2 = Student {neptunID = 102, studentName = "Bob", uni = univ1, grades = [88, 92, 80]}

student3 = Student {neptunID = 103, studentName = "Charlie", uni = univ2, grades = [75, 85, 82]}

student4 = Student {neptunID = 104, studentName = "David", uni = univ2, grades = [90, 87, 91]}

student5 = Student {neptunID = 105, studentName = "Eve", uni = univ3, grades = [80, 83, 88]}

student6 = Student {neptunID = 106, studentName = "Frank", uni = univ3, grades = [85, 89, 84]}

student7 = Student {neptunID = 103, studentName = "Charlie1", uni = univ2, grades = [75, 59, 82]}

student8 = Student {neptunID = 105, studentName = "Eve1", uni = univ3, grades = [80, 9, 8]}


-------------------------------------------------------

dotProd :: [Int] -> [Int] -> Int
dotProd lst1 lst2 = sum ([ x * y | x <- lst1 | y <- lst2])

-- main = print $ dotProd [1, 2, 3] [4, 5, 6] -- 32

lightStates :: String -> String -> [String]
lightStates signal1 signal2 = map (\pair-> if pair == "11" then "Off" else "On") multiplied
    where multiplied = [ [x,y] | x <- signal1 | y <- signal2 ]

-- main = print $ lightStates "1100" "1010" -- ["On", "Off", "Off", "Off"]


failingStudents :: [Student] -> [(String, [Int])]
failingStudents lst = [ (studentName s, [ g | g <- grades s, g < 60 ]) | s <- lst, [ g | g <- grades s, g < 60 ] /= [] ]

-- main = print $ failingStudents [student1, student2, student7, student4, student8, student6] 

topScorersInTeam :: Team -> [Player] -> [Player]
topScorersInTeam team1 playersList = [ p | p <- filtered, totalScore p == maxScore ]
    where 
        totalScoreOnePlayer p = sum (playerScores p)
        filtered = [ p | p <- playersList, teamName team1 == teamName (playerTeam p)]
        maxScore  = maximum (0 : [ totalScore p | p <- filtered ])







