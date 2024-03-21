#! /usr/bin/awk -f 

BEGIN {}
{
    
FS=";"

split($2, puzzleInfo, ":")
difficulty = puzzleInfo[2]

split($1, date, "-")
puzzleNumber = date[3]

# Create a new text file for each puzzle
fileName = "sudokus/" difficulty puzzleNumber ".txt"
print $6 > fileName
}