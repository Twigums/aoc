import numpy as np

seeds, seed_soil, soil_fert, fert_water, water_light, light_temp, temp_humid, humid_loc = open("input").read().split("\n\n")

seeds = seeds[7:].split(" ")
for i, seed in enumerate(seeds):
    seeds[i] = int(seed)

def fix_format(mat):
    mat = mat.split("\n")[1:]
    for i, line in enumerate(mat):
        mat[i] = line.split(" ")

    return mat

def find_spot(num, mat):
    for line in mat:
        dest, start, gap = line
        dest = int(dest)
        start = int(start)
        gap = int(gap)

        if num >= start and start + gap > num:
            dist = num - start
            spot = dest + dist

            return spot

        else:
            spot = num

    return spot

def get_lists(lists, mat):
    res_lists = []

    for ranges in lists:
        val0, val1 = ranges

        for line in mat:
            no_ranges = False
            dest, start, gap = line
            dest = int(dest)
            start = int(start)
            gap = int(gap)

            if start <= val0 and start + gap > val1:
                case1 = [dest + val0 - start, dest + val0 - start + (val1 - val0)]
                res_lists.append(case1)

            elif start <= val0 and start + gap >= val0 and start + gap <= val1:
                case2 = [dest + val0 - start, dest + gap]
                res_lists.append(case2)

            elif start >= val0 and start + gap < val1:
                case3 = [dest, dest + gap]
                res_lists.append(case3)

            elif start > val0 and start <= val1 and start + gap > val1:
                case4 = [dest, dest + (val1 - start)]
                res_lists.append(case4)

            else:
                no_ranges = True

        if no_ranges == True:
            no_ranges = False
            case5 = ranges
            res_lists.append(ranges)

    return res_lists




ans = 10000000000000000

seed_soil = fix_format(seed_soil)
soil_fert = fix_format(soil_fert)
fert_water = fix_format(fert_water)
water_light = fix_format(water_light)
light_temp = fix_format(light_temp)
temp_humid = fix_format(temp_humid)
humid_loc = fix_format(humid_loc)

seed_lists = []

total2 = []

for i in range(int(len(seeds) / 2)):
    seed_lists.append([seeds[2 * i], seeds[2 * i] + seeds[2 * i + 1]])

soil_lists = get_lists(seed_lists, seed_soil)
fert_lists = get_lists(soil_lists, soil_fert)
water_lists = get_lists(fert_lists, fert_water)
light_lists = get_lists(water_lists, water_light)
temp_lists = get_lists(light_lists, light_temp)
humid_lists = get_lists(temp_lists, temp_humid)
loc_lists = get_lists(humid_lists, humid_loc)

for loc_range in loc_lists:
    total2.append(loc_range[0])

for seed in seeds:
    soil_spot = find_spot(seed, seed_soil)
    fert_spot = find_spot(soil_spot, soil_fert)
    water_spot = find_spot(fert_spot, fert_water)
    light_spot = find_spot(water_spot, water_light)
    temp_spot = find_spot(light_spot, light_temp)
    humid_spot = find_spot(temp_spot, temp_humid)
    loc_spot = find_spot(humid_spot, humid_loc)

    if loc_spot < ans:
        ans = loc_spot

print(ans)
print(min(total2))

