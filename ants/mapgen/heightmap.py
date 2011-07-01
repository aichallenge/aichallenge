#!/usr/bin/env python
from map import *
from random import randint
from collections import defaultdict

class HeightMapMap(Map):
    def __init__(self, options={}):
        super(HeightMapMap, self).__init__(options)
        self.name = 'height_map'
        self.rows = options.get('rows', (40,200))
        self.cols = options.get('cols', (40,200))
        self.players = options.get('players', (2,12))
        self.land = options.get('land', (75, 85))
    
    def generate(self):
        rows = self.get_random_option(self.rows)
        cols = self.get_random_option(self.cols)
        
        # calc max players that can be tiled
        row_max = rows//16
        col_max = cols//16
        player_max = row_max*col_max
        
        # fix dimensions for tiling evenly
        players =self.get_random_option((self.players[0],
                                         min(player_max, self.players[1])))
        # pick random grid size
        divs = [(i, players//i) for i in range(1,min(players+1, row_max+1))
                if players % i == 0        # look fox divisors of players
                #and i < row_max           # ensure grid rows < row_max
                and players//i < col_max  # ensure grid cols < col_max
                ]
        if len(divs) == 0:
            return self.generate()
        row_sym, col_sym = choice(divs)
        
        # fix dimensions for even tiling
        rows //= row_sym
        cols //= col_sym            
        
        land = self.get_random_option(self.land)
        

        # initialize height map
        height_map = [[0]*cols for _ in range(rows)]
    
        # cut and lift
        iterations = 100
        for _ in range(iterations):
            row = randint(0, rows-1)
            col = randint(0, cols-1)
            radius = randint(5, (rows+cols)/4)
            radius2 = radius**2
            for d_row in range(-radius, radius+1):
                for d_col in range(-radius, radius+1):
                    h_row = (row + d_row) % rows
                    h_col = (col + d_col) % cols
                    if self.euclidean_distance2((row, col), (h_row, h_col), (rows, cols)) <= radius2:
                        height_map[h_row][h_col] += 1
        
        # create histogram
        histo = defaultdict(int)
        for height_row in height_map:
            for height in height_row:
                histo[height] += 1
    
        # find sea level
        map_area = rows * cols
        land_area = 0
        for height in sorted(histo.keys(), reverse=True):
            land_area += histo[height]
            if 1.0 * land_area / map_area >= 1.0 * land / 100:
                break
            
        # initialize map
        self.map = [[LAND]*cols for _ in range(rows)]
                
        # place water    
        for row in range(rows):
            for col in range(cols):
                if height_map[row][col] < height:
                    self.map[row][col] = WATER
        self.fill_small_areas()
        
        # check too make sure too much wasn't filled in
        areas = self.section(0)
        land_area = len(areas[0][0])
        percent = 1.0 * land_area / map_area
        #print(land_area, map_area, percent)
        if percent < 1.0 * land / 100:
            return self.generate()
        
        # place player start
        while self.map[row][col] != LAND:
            row = randint(0, rows-1)
            col = randint(0, cols-1)
        self.map[row][col] = ANTS
        
        # tile map
        t_rows = rows * row_sym
        t_cols = cols * col_sym
        ant = 0
        map = [[LAND]*t_cols for _ in range(t_rows)]
        for t_row in range(t_rows):
            for t_col in range(t_cols):
                row = t_row % rows
                col = t_col % cols
                map[t_row][t_col] = self.map[row][col]
                if self.map[row][col] == ANTS:
                    map[t_row][t_col] = ant
                    ant += 1
        self.map = map
        self.make_wider()
        
def main():
    new_map = HeightMapMap()
    new_map.generate()
    
    # check that all land area is accessable
    while new_map.allowable() != None:
        #print(new_map.allowable())
        new_map.generate()
        
    new_map.toText()
    
if __name__ == '__main__':
    main()