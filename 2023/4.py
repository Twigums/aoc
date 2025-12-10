import numpy as np

f = open("input").read().split("\n")

total = 0
cards = [1 for i in range(len(f))]

for i, card in enumerate(f):
    card = card.split(":")
    card = card.pop(-1)

    win_num, num = card.split("|")
    win_num = win_num.split(" ")
    num = num.split(" ")

    while "" in win_num:
        win_num.remove("")

    while "" in num:
        num.remove("")

    for j, number in enumerate(win_num):
        win_num[j] = int(number)

    count = -1

    for number in num:
        if int(number) in win_num:
            count += 1

    if count != -1:
        total += 2 ** count

    # part 2
    for j in range(i + 1, i + count + 2):
        cards[j] += cards[i] * 1

total2 = sum(cards)

print(total)
print(total2)
