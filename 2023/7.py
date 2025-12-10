f = open("input").read().split("\n")

order = "J123456789TQKA"

card_info = {0: [], 1: [], 2: [], 3: [], 32: [], 40: [], 50: []}

# 5 of a kind = 50
# 4 of a kind = 40
# full house = 32
# 3 of a kind = 3
# two pair = 2
# one pair = 1
# high card = 0

for i, string in enumerate(f):
    card_list = [0 for j in range(14)]
    cards, bet = string.split(" ")
    card_ints = []

    J_exists = False
    for card in cards:

        if card == "J":
            J_exists = True

            for i in range(len(card_list)):
                card_list[i] += 1

        else:
            card_list[order.find(card)] += 1

        card_ints.append(order.find(card))

    card_ints.append(int(bet))

    if max(card_list) == 5:
        card_info[50].append(card_ints)

    if max(card_list) == 4:
        card_info[40].append(card_ints)

    if max(card_list) == 3:
        three_counts = 0
        for val in card_list:
            if val == 3:
                three_counts += 1

        if three_counts == 2:
            card_info[32].append(card_ints)

        elif 2 in card_list and not J_exists:
            card_info[32].append(card_ints)

        else:
            card_info[3].append(card_ints)

    if max(card_list) == 2:
        two_counts = 0
        for val in card_list:
            if val == 2:
                two_counts += 1

        if two_counts == 2:
            card_info[2].append(card_ints)

        else:
            card_info[1].append(card_ints)


    if max(card_list) == 1:
        card_info[0].append(card_ints)

total = 0
rank = 1

for val in card_info:
    card_info[val].sort()

    for card in card_info[val]:
        total += rank * card[-1]
        rank += 1

print(total)
