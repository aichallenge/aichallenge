#!/usr/bin/env python
from planetwars import *

def do_turn(pw):
    my_strongest = -1
    their_weakest = 99999999
    for x in range (0, len(pw.planets)):
        planet = pw.planets[x]
        if planet.owner == 1 and planet.num_ships > my_strongest:
            my_strongest = x
        elif planet.owner != 1 and planet.num_ships < their_weakest:
            their_weakest = x

    if my_strongest != -1 and their_weakest != 99999999:
        pw.issue_order(my_strongest,
                       their_weakest,
                       pw.planets[my_strongest].num_ships / 2)
    pw.end_turn()

def main():
    map_data = ""
    while True:
        line = StringUtils.input()
        if line[0:2] == "go":
            do_turn(PlanetWars(map_data))
            map_data = ""
        else:
            map_data += line

if __name__ == "__main__":
    main()
