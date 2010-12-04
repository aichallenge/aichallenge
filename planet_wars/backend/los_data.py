#!/usr/bin/python

import sys

def parse_los(lines):
    header = lines[0]
    width = len(header.split()[0]) + 1
    name_width = len(header) - len(header.lstrip()) - 1
    divisor = 10 ** (float(width) - 1)
    los_data = []
    for line in lines[1:]:
        name = line[0:name_width]
        field_str = line[name_width:]
        fields = []
        for i in range(0, len(field_str) - width, width):
            f = field_str[i:i+width]
            try:
                fields.append(int(f) / divisor)
            except ValueError:
                if f.strip() == "":
                    fields.append(None)
                else:
                    raise
        los_data.append((name, fields))
    return (los_data, name_width, width)

def main(los_filename, leaderboard=None):
    los_file = open(los_filename, 'r')
    los_lines = los_file.readlines()
    los_file.close()
    los_data, name_width, width = parse_los(los_lines)
    rank = 0
    print "Rank Name%*s  10%%  90%%" % (name_width - 17, " ")
    for user, probabilities in los_data:
        rank += 1
        user_id, name, sub_id = user.split(',')
        sub_id = int(sub_id)
        self = probabilities.index(None)
        num_before = len(probabilities[:self])
        upper_limit = num_before + 1
        for pr_rank, per in enumerate(probabilities[:self]):
            if per >= 0.10:
                break
            upper_limit = num_before - pr_rank
        lower_limit = 1
        for dist, per in enumerate(probabilities[self+1:]):
            if per < 0.90:
                lower_limit = dist + 2
        print "%4d %-*s %4d %4d" % (rank, name_width-13, name,
                upper_limit, lower_limit)

if __name__ == "__main__":
    if len(sys.argv) < 2:
        print "usage: %s <los file> [leaderboard id]" % (sys.argv[0],)
        sys.exit()
    leaderboard = None
    if len(sys.argv) > 2:
        leaderboard = int(sys.argv[2])
    main(sys.argv[1], leaderboard)

