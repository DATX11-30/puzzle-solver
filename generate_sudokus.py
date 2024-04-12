import os

def main(): 
    with open('SudokuPuzzles2024.csv', 'r') as f:
        lines = f.readlines()
        for line in lines:
            l = line.split(';')
            difficulty = l[1].split(' ')[-1]
            date = l[0].replace('-', '_')
            filename = difficulty + '_' + date
            print(filename + '.txt')
            
            directory = './sudokus/' + difficulty
            if not os.path.exists(directory):
                os.makedirs(directory)

            with open('./sudokus/' + difficulty + '/' + filename + '.txt', 'w') as s:
                s.write(l[5] + '\n')
                s.write(l[6])
            


if (__name__ == "__main__"):
     main()