f = open("input").read().split("\n")

time = f[0]
dist = f[1]

time = time[11:].split(" ")
dist = dist[11:].split(" ")

time_formatted = []
dist_formatted = []
time2 = ""
dist2 = ""

def find_dist(max_time, max_dist):
    val = 0
    for i in range(1, max_time):
        total_dist = (max_time - i) * i

        if total_dist > max_dist:
            val += 1

    return val

for string in time:
    if string != "":
        time_formatted.append(int(string))
        time2 += string

for string in dist:
    if string != "":
        dist_formatted.append(int(string))
        dist2 += string

wins = [0 for i in range(len(time_formatted))]

for i, max_dist in enumerate(dist_formatted):

    max_time = time_formatted[i]
    wins[i] = find_dist(max_time, max_dist)

total = 1
for val in wins:
    total *= val

total2 = find_dist(int(time2), int(dist2))

print(total)
print(total2)
