#!/usr/bin/python
from random import randrange

LAND = 0
FOOD = -1
WALL = -2

def random_box():
    players = 4
    width = randrange(16, 64) * 2
    height = randrange(16, 64) * 2
    m = [[WALL for x in range(width)] for y in range(height)]
    def carve(row, col):
        if m[row][col] == WALL:
            m[row][col] = LAND
    for box in range(randrange(7,14)):
        l = randrange(width-5)
        t = randrange(height-5)
        r = randrange(l+2, min(l+width//4,width))
        b = randrange(t+2, min(t+height//4,height))
        for y in range(t, b+1):
            for x in range(l, r+1):
                carve(y, x)
        for y in range(height-b-1, height-t):
            for x in range(l, r+1):
                carve(y, x)
        for y in range(t, b+1):
            for x in range(width-r-1, width-l):
                carve(y, x)
        for y in range(height-b-1, height-t):
            for x in range(width-r-1, width-l):
                carve(y, x)
        if box == 0:
            m[t][l] = 1
            m[height-t-1][l] = 2
            m[t][width-l-1] = 3
            m[height-t-1][width-l-1] = 4
            for y in range(t+1, height-t-1):
                carve(y, l)
                carve(y, width-l-1)
            for x in range(l+1, width-l-1):
                carve(t, x)
                carve(height-t-1, x)
    return ant_map(m)

def ant_map(m):
    tmp = 'D %s %s 4\n' % (len(m[0]), len(m))
    for row in m:
        tmp += 'M '
        for col in row:
            if col == LAND:
                tmp += '.'
            elif col == WALL:
                tmp += 'X'
            else:
                tmp += chr(col + 96)
        tmp += '\n'
    return tmp
    
if __name__ == '__main__':
    print(random_box())