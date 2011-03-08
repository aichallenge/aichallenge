#!/usr/bin/env python

from collections import deque, defaultdict

#returns the new location after moving in a particular direction
directions = {'N': (-1,0), 'S': (1,0), 'E': (0,1), 'W': (0,-1)}
def get_loc(loc, direction, rows, cols):                                    
    dr, dc = directions[direction]                              
    return [(loc[0]+dr)%rows, (loc[1]+dc)%cols ]

def analyze_map(map_location):
    #variables
    rows = cols = no_players = 0
    map = []
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
            map.append(tokens[1])
    map_file.close()

    #checks to see that the map is of the correct dimensions
    if len(map) != rows:
        raise ValueError("incorrect number of rows given")
    for row in range(len(map)):
        if len(map[row]) != cols:
            raise ValueError("incorrect number of columns in row " + str(row))
            
    #gets information about the map
    for row in range(len(map)):
        for col in range(len(map[row])):
            if map[row][col] == '.':
                no_land_squares += 1
            elif map[row][col] == '%':
                no_water_squares += 1
            elif map[row][col] == '*':
                no_food_squares += 1
            elif map[row][col] >= 'a' and map[row][col] <= 'z':
                if not map[row][col] in players:
                    players.append(map[row][col])
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

    for row in range(len(map)):
        for col in range(len(map[row])):
            if map[row][col] >= 'a' and map[row][col] <= 'z':
                p = ord(map[row][col])-97
                ant_counts[p] += 1
                access_map[row][col] = [0, [p], [ [row, col] ] ]
                square_queue.append( [row, col] )


    #finds information about who can reach what land and food squares first	
    while len(square_queue) > 0:
        cLoc = square_queue.popleft()
        cPlayers = access_map[cLoc[0] ][cLoc[1] ][1]
        
        for d in directions:
            nLoc = get_loc(cLoc, d, rows, cols)
            
            if map[nLoc[0] ][nLoc[1] ] != '%':
                if access_map[nLoc[0] ][nLoc[1] ][0] == -1: #first time reached
                    access_map[nLoc[0]][nLoc[1]][0] = access_map[cLoc[0]][cLoc[1]][0] + 1
                    access_map[nLoc[0] ][nLoc[1] ][1] += cPlayers
                    square_queue.append(nLoc)
                elif access_map[nLoc[0]][nLoc[1]][0] == access_map[cLoc[0]][cLoc[1]][0] + 1:
                    for p in cPlayers:
                        if not p in access_map[nLoc[0]][nLoc[1]][1]:
                            access_map[nLoc[0] ][nLoc[1] ][1].append(p)
                    access_map[nLoc[0]][nLoc[1]][1].sort()

    #outputs the partitioned map						
    '''for row in range(len(access_map)):
        for col in range(len(access_map[row])):
            if map[row][col] == '%':
                print '%',
            elif len(access_map[row][col][1]) == 1:
                print chr(int(access_map[row][col][1][0]) + 97),
            else:
                print len(access_map[row][col][1]),
        print'''
            
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

    return result

if __name__ == '__main__':
    print analyze_map("amap.map")
