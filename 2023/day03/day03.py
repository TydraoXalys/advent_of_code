def find_nearest_symbols(data, row, col):
    neighbors = [
        (-1, -1), (-1, 0), (-1, 1),
        (0, -1) ,          (0, 1) ,
        (1, -1) , (1, 0) , (1, 1)
    ]
    number = ''
    nb_symbols_found = 0
    dj = 0
    while data[row][col+dj] in '0123456789':
        number += data[row][col+dj]
        for (dr,dc) in neighbors:
            neighbor_row, neighbor_col = row+dr, col+dj+dc
            if 0 <= neighbor_row < len(data) and 0 <= neighbor_col < len(data[row]):
                if data[neighbor_row][neighbor_col] not in '.0123456789':
                    nb_symbols_found += 1
        dj += 1
        if col+dj == len(data[row]):
            break
    return int(number), len(number), nb_symbols_found
            
def compute_gear(data):
    gear_numbers = []
    for i in range(len(data)):
        number = ''
        for j in range(len(data[0])):
            if data[i][j] in '0123456789':
                number += data[i][j]
            else:
                if number != '' and (3 <= j <= 5 or (j==6 and len(number)>1)) :
                    gear_numbers.append(number)
                number = ''
        if len(number) == 3:
            gear_numbers.append(number)
    if len(gear_numbers) == 2:
        return int(gear_numbers[0])*int(gear_numbers[1])
    return 0

def day3():
    with open("day03/puzzle_input.txt","r") as input_file:        
        
        sum = 0
        gear_ratios = 0
        data = [line.strip() for line in input_file.readlines()]
        
        for i in range(len(data)):
            j = 0
            while j < len(data[0]):
                if data[i][j] in '0123456789':
                    number, incr, nb_symbols_found = find_nearest_symbols(data, i, j)
                    if nb_symbols_found > 0:
                        sum += number
                    j += incr
                elif data[i][j] == '*':
                    gear_ratios += compute_gear([data[i-1][j-3:j+4],data[i][j-3:j+4],data[i+1][j-3:j+4]])
                    j += 1
                else:
                    j += 1
                    
        return sum, gear_ratios

print(day3())