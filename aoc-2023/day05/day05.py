from concurrent.futures import ThreadPoolExecutor
import re

def computeSeedLocation(almanac, seed):
    print(seed)
    location = int(seed)
    for i in range(len(almanac)):
        for j in range(len(almanac[i])):
            start = int(almanac[i][j][1])
            destination = int(almanac[i][j][0])
            range_length = int(almanac[i][j][2])
            if start <= location < start+range_length:
                location = destination + location-start
                break
    return location

def day05():
    with open("day05/puzzle_input.txt","r") as input_file:        
        lines = input_file.readlines()
        seeds = re.findall("\d+",lines[0])
        almanac = []
        locations = []
        
        temp = []
        for i in range(2, len(lines)):
            line = lines[i].strip()
            if line == "" or i==len(lines)-1:
                temp.sort(key= lambda elem: elem[0])
                almanac.append(temp)
                temp = []
            elif re.search("map", line)==None:
                temp.append(line.split(" "))  
                
        for seed in seeds:
            locations.append(computeSeedLocation(almanac, seed))
        part1 = min(locations)
        
        locations.clear()
        
        for i in range(0, len(seeds), 2):
            start_seed = int(seeds[i])
            seed_range = int(seeds[i+1])
            for seed in range(start_seed, start_seed+seed_range):
                locations.append(computeSeedLocation(almanac, seed))
        part2 = min(locations)
        
        return part1, part2
        

print(day05())