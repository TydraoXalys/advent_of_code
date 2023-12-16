def day04():
    with open("day04/puzzle_input.txt","r") as input_file:
        lines = input_file.readlines()
        cards = []
        for line in lines:
            card = line.strip().split(": ")[1].split(" | ")
            numbers = set(card[0].split(" ")).intersection(set(card[1].split(" ")))
            numbers.discard('')
            cards.append(numbers)
            
        points = sum([2**(len(card)-1) if len(card)>0 else 0 for card in cards])
        
        scratchcards = [ 1 for i in range(len(cards))]
        for i in range(len(cards)):
            for j in range(i+1, min(len(cards),i+1+len(cards[i]))):
                scratchcards[j] += scratchcards[i]
    return points, sum(scratchcards)

print(day04())