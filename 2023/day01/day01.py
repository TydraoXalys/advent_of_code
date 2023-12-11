def day1():
    with open("day01/puzzle_input.txt","r") as input_file:
        sum = 0
        digits = ["zero","one","two","three","four","five","six","seven","eight","nine"]+[str(i) for i in range(10)]
        for line in input_file.readlines():
            unit = -1
            tens = -1
            i = 0
            line = line[:-1]+"----"
            while i < len(line)-4:
                temp = line[i:i+5]
                for digit in digits:
                    if temp[:len(digit)] == digit:
                        tens = digits.index(digit)%10
                        break
                if tens != -1:
                    break
                i += 1
            
            i = len(line)-5
            while i >= 0:
                temp = line[i:i+5]
                for digit in digits:
                    if temp[:len(digit)] == digit:
                        unit = digits.index(digit)%10
                        break
                if unit != -1:
                    break
                i -= 1    
                
            sum += tens*10+unit
    return sum
                
print(day1())