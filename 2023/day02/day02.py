def day2():
    with open("day02/puzzle_input.txt","r") as input_file:
        sum = 0
        power = 0
        colors = {}
        for line in input_file.readlines():
            colors["red"],colors["green"],colors["blue"] = 0,0,0
            game = line.strip().split(": ")
                        
            for elem in game[1].split("; "):
                set_colors = elem.split(", ")
                for set_color in set_colors:
                    value_key = set_color.split(" ")
                    colors[value_key[1]] = max(colors[value_key[1]], int(value_key[0]))
            
            if colors["red"] <= 12 and colors["green"] <= 13 and colors["blue"] <= 14:
                sum += int(game[0][5:])
            power += colors["red"]*colors["green"]*colors["blue"]
    return sum, power

print(day2())