def day04():
    with open("day04/puzzle_input.txt","r") as input_file:
        points = 0
        lines = input_file.readlines()
        cards = []
        for line in lines:
            card = line.strip().split(": ")
            numbers = card[1].split(" | ")
            numbers[0] = numbers[0].split(" ")
            numbers[1] = numbers[1].split(" ")
            cards.append(numbers)
        for card in cards:
            card_point = 0
            for nb in card[0]:
                if nb != '' and nb in card[1]:
                    if card_point == 0:
                        card_point = 1
                    else:
                        card_point *= 2
            points += card_point
        
        scratchcards = [ 1 for i in range(len(cards))]
        for i in range(len(cards)):
            count = 0
            for nb in cards[i][0]:
                count += (nb != '' and nb in cards[i][1])
            if count > 0:
                for j in range(i+1, min(len(cards),i+1+count)):
                    scratchcards[j] += scratchcards[i]
    return points, sum(scratchcards)

print(day04())