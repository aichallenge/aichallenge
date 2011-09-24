#!/usr/bin/env python

from collections import deque, defaultdict
import sys

#returns the new location after moving in a particular direction
directions = {'N': (-1,0), 'S': (1,0), 'E': (0,1), 'W': (0,-1)}
def get_loc(loc, direction, rows, cols):                                    
    dr, dc = directions[direction]                              
    return [(loc[0]+dr)%rows, (loc[1]+dc)%cols ]

def analyze_map(map_location):
    #variables
    rows = cols = no_players = 0
    map_data = []
    players = []

    given_no_players = -1
    player_line_given = False

    no_land_squares = no_water_squares = no_food_squares = 0

    #reads the data from the map file
    map_file = open(map_location, 'r')
    for line in map_file.readlines():
        tokens = line.split()
        
        if tokens[0] == "rows":
            rows = int(tokens[1])
        elif tokens[0] == "cols":
            cols = int(tokens[1])
        elif tokens[0] == "players":
            player_line_given = True
            given_no_players = int(tokens[1])
        elif tokens[0] == "m":
            map_data.append(tokens[1])
    map_file.close()

    #checks to see that the map is of the correct dimensions
    if len(map_data) != rows:
        raise ValueError("incorrect number of rows given")
    for row in range(len(map_data)):
        if len(map_data[row]) != cols:
            raise ValueError("incorrect number of columns in row " + str(row))
            
    #gets information about the map
    for row in range(len(map_data)):
        for col in range(len(map_data[row])):
            if map_data[row][col] == '.':
                no_land_squares += 1
            elif map_data[row][col] == '%':
                no_water_squares += 1
            elif map_data[row][col] == '*':
                no_food_squares += 1
            elif map_data[row][col] >= 'a' and map_data[row][col] <= 'z':
                if not map_data[row][col] in players:
                    players.append(map_data[row][col])
                    no_players += 1
            else:
                raise ValueError("incorrect square value given")

    #checks the correct number of players were given
    if player_line_given and no_players != given_no_players:
        raise ValueError("wrong number of players specified")
        
    #checks the correct players were given
    players.sort()
    expected = 'a'
    for player in players:
        if player != expected:
            raise ValueError("player " + str(expected) + " not given")
        expected = chr(ord(expected)+1)	

    #finds information about where players are
    ant_counts = defaultdict(int)
    access_map = [ [ [-1, [] ] for c in range(cols)] for r in range(rows) ]
    square_queue = deque([])

    for row in range(len(map_data)):
        for col in range(len(map_data[row])):
            if map_data[row][col] >= 'a' and map_data[row][col] <= 'z':
                p = ord(map_data[row][col])-97
                ant_counts[p] += 1
                access_map[row][col] = [0, [p], [ [row, col] ] ]
                square_queue.append( [row, col] )


    #finds information about who can reach what land and food squares first	
    while len(square_queue) > 0:
        c_loc = square_queue.popleft()
        c_players = access_map[c_loc[0] ][c_loc[1] ][1]
        
        for d in directions:
            n_loc = get_loc(c_loc, d, rows, cols)
            
            if map_data[n_loc[0] ][n_loc[1] ] != '%':
                if access_map[n_loc[0] ][n_loc[1] ][0] == -1: #first time reached
                    access_map[n_loc[0]][n_loc[1]][0] = access_map[c_loc[0]][c_loc[1]][0] + 1
                    access_map[n_loc[0] ][n_loc[1] ][1] += c_players
                    square_queue.append(n_loc)
                elif access_map[n_loc[0]][n_loc[1]][0] == access_map[c_loc[0]][c_loc[1]][0] + 1:
                    for p in c_players:
                        if not p in access_map[n_loc[0]][n_loc[1]][1]:
                            access_map[n_loc[0] ][n_loc[1] ][1].append(p)
                    access_map[n_loc[0]][n_loc[1]][1].sort()
            
    #works out access counts
    land_access_counts = defaultdict(int)

    for row in access_map:
        for cell in row:
            t = cell[1]
            if len(t) >= 1:
                land_access_counts[tuple(t)] += 1

    #build dictionary of information
    result = {'players': no_players,
        'rows': rows,
        'cols': cols,
        'counts': ant_counts,
        'space': land_access_counts
    }
    sys.stdout.write("# " + str(result) + "\n")

    #outputs the partitioned map_data
    sys.stdout.write("players " + str(no_players) + "\n")                        
    sys.stdout.write("rows " + str(rows) + "\n")                        
    sys.stdout.write("cols " + str(cols) + "\n")                        
    for row in range(len(access_map)):
        sys.stdout.write("m ")
        for col in range(len(access_map[row])):
            if map_data[row][col] == '%':
                sys.stdout.write('%')
            elif len(access_map[row][col][1]) == 1:
                sys.stdout.write( chr(int(access_map[row][col][1][0]) + 97))
            else:
                #sys.stdout.write(str( len(access_map[row][col][1])))
                sys.stdout.write(".")
        sys.stdout.write('\n')

    
    return result

if __name__ == '__main__':
    analyze_map(sys.argv[1])
