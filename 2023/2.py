f = open("input").read().split("\n")

# part 1
color_dict = {
        "red": 12,
        "green": 13,
        "blue": 14,
        }

summed_ids = 0
summed_power = 0

for string in f:
    # part 2
    empty_dict = {
        "red": 0,
        "green": 0,
        "blue": 0,
        }

    viable = True
    game, sets = string.split(":")

    game_id = int(game[5:])
    sets = sets.split(";")

    for game_set in sets:
        cubes = game_set.split(",")

        for cube in cubes:
            _, val, color = cube.split(" ")

            if int(val) > color_dict[color]:
                viable = False

            # part 2
            if int(val) > empty_dict[color]:
                empty_dict[color] = int(val)

    if viable == True:
        summed_ids += game_id


    # part 2
    power = empty_dict["red"] * empty_dict["green"] * empty_dict["blue"]
    summed_power += power

print(summed_ids)
print(summed_power)
