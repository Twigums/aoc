import numpy as np

f = open("input").read().split("\n")

nums = ["1", "2", "3", "4", "5", "6", "7", "8", "9", "0"]

range_mat = np.array([
    [-1, -1],
    [1, 1],
    ])

engine_arr = []
total = 0
total2 = 0

for line in f:
    line_list = []
    for char in line:
        line_list.append(char)

    engine_arr.append(line_list)

engine_arr = np.array(engine_arr)
row, col = engine_arr.shape

number = ""
number_idx_range = []
start = True

saved_stars = []
saved_numbers = []

for i in range(row):

    for j in range(col):
        char = engine_arr[i, j]

        if char in nums:
            number += char

            if start:
                start = False
                number_idx_range.append([i, j])

        if char not in nums or j + 1 == col:
            start = True

            number_idx_range.append([i, j - 1])
            number_idx_range = np.array(number_idx_range)

            if number != "":
                idx_range = number_idx_range + range_mat

                for idx_row in idx_range:
                    if idx_row[0] < 0:
                        idx_row[0] = 0
                    if idx_row[1] < 0:
                        idx_row[1] = 0
                    if idx_row[0] > row:
                        idx_row[0] = row
                    if idx_row[1] > col:
                        idx_row[1] = col

                start_row = idx_range[0, 0]
                start_col = idx_range[0, 1]
                row_slice = idx_range[1, 0] - idx_range[0, 0] + 1
                col_slice = idx_range[1, 1] - idx_range[0, 1] + 1

                number_border_unflattened = engine_arr[start_row:start_row + row_slice, start_col:start_col + col_slice]
                number_border = number_border_unflattened.flatten()

                unique = []

                for char in number_border:
                    if char not in unique:
                        unique.append(char)

                for char in unique:
                    if char != "." and char not in nums:
                        total += int(number)

                border_row, border_col = number_border_unflattened.shape

                for a in range(border_row):
                    for b in range(border_col):
                        char = number_border_unflattened[a, b]

                        if char == "*":
                            current_star = idx_range[0] + [a, b]
                            saved_stars.append(list(current_star))
                            saved_numbers.append(int(number))

            number = ""
            number_idx_range = []

temp_stars = np.array(saved_stars)
duplicates = {}

for i, star in enumerate(saved_stars):
    star = tuple(star)
    if star not in duplicates:
        duplicates[star] = [i]

    else:
        duplicates[star].append(i)

for star in duplicates:
    idx_list = duplicates[star]
    if len(idx_list) == 2:
        total2 += saved_numbers[idx_list[0]] * saved_numbers[idx_list[1]]

print(total)
print(total2)
