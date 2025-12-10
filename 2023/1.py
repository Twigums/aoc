import numpy as np

f = open("input").read().split("\n")

nums = ["1", "2", "3", "4", "5", "6", "7", "8", "9", "0"]
words = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]

total = 0

for string in f:
    final = ""

    used_num = []
    all_idx = []

    # part 1
    for i, char in enumerate(string):
        if char in nums:
            used_num.append(char)
            all_idx.append(i)

    for i, word in enumerate(words):

        str_idx = [j for j in range(len(string)) if string.find(word, j) == j]

        for idx in str_idx:
            if idx != -1:
                used_num.append(nums[i])
                all_idx.append(idx)

    if used_num != []:
        final += used_num[np.argmin(all_idx)]
        final += used_num[np.argmax(all_idx)]

        total += int(final)

print(total)
