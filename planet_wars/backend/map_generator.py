#!/usr/bin/python

import math
import random

# minimum and maximum total number of planets in map
minPlanets = 15
maxPlanets = 30
# maximum number of planets located equidistant from both players
# not including the planet exactly in the center of the map
maxCentral = 5
# minimum and maximum number of ships on neutral planets
minShips = 1
maxShips = 100
# minimum and maximum growth for planets
# except for the center planet which is always 0 minimum growth
minGrowth = 1
maxGrowth = 5
# minimum distance between planets
minDistance = 2
# minimum distance between the players starting planets
minStartingDistance = 4
# maximum radius from center of map a planet can be
maxRadius = 12
# minimum difference between true distance and rounded distance between planets
# this is to try and avoid rounding errors causing different distances to be
# calculated on different platforms and languages
epsilon = 0.002

def make_planet(x, y, owner, num_ships, growth_rate):
    return {"x" : x, "y" : y, "owner" : owner, "num_ships" : num_ships,
            "growth_rate" : growth_rate}

def print_planet(p):
    out = ["P", p["x"], p["y"], p["owner"], p["num_ships"], p["growth_rate"]]
    print " ".join(str(i) for i in out)

def translate_planets(planets):
    for p in planets:
        p["x"] += maxRadius+2
        p["y"] += maxRadius+2

def generate_coordinates(p, r, theta):
    if theta < 0:
        theta += 360
    if theta >= 360:
        theta -= 360
    p["x"] = r * math.cos( math.radians(theta) )
    p["y"] = r * math.sin( math.radians(theta) )

def rand_num(min, max):
    return ( random.random() * (max-min) ) + min

def rand_radius(min, max):
    return ( math.sqrt(random.random()) * (max-min) ) + min

def distance(p1, p2):
    return math.ceil(actual_distance(p1, p2))

def actual_distance(p1, p2):
    dx = p1["x"] - p2["x"]
    dy = p1["y"] - p2["y"]
    return math.sqrt(dx * dx + dy * dy)

def not_valid(p1, p2):
    adist = actual_distance(p1, p2)
    if distance(p1, p2) < minDistance or abs(adist - round(adist)) < epsilon:
        return True
    for p in planets:
        adist1 = actual_distance(p, p1)
        adist2 = actual_distance(p, p2)
        if (distance(p, p1) < minDistance
                or distance(p, p2) < minDistance
                or abs(adist1-round(adist1)) < epsilon
                or abs(adist2-round(adist2)) < epsilon):
            return True
    return False

def not_valids(p1):
    for p in planets:
        adist = actual_distance(p, p1)
        if distance(p, p1) < minDistance or abs(adist-round(adist)) < epsilon:
            return True
    return False

#works out information about the map
planetsToGenerate = random.randint(minPlanets, maxPlanets)
if random.randint(0, 1):
    symmetryType = 1 # radial symmetry
    # can only generate an odd number of planets in this symmetry
    while planetsToGenerate % 2 == 0:
        if planetsToGenerate == maxPlanets:
            planetsToGenerate = minPlanets
        else:
            planetsToGenerate += 1
else:
    symmetryType = -1 # linear symmetry

planets = []

#adds the centre planet
planets.append(make_planet(0, 0, 0, random.randint(minShips, maxShips),
    random.randint(0, maxGrowth)))
planetsToGenerate -= 1

#picks out the home planets
r = rand_radius(minDistance, maxRadius)
theta1 = rand_num(0, 360)
if symmetryType == 1 and theta1 < 180:
    theta2 = theta1+180
elif symmetryType == 1:
    theta2 = theta1-180
else:
    theta2 = rand_num(0, 360)

p1 = make_planet(0, 0, 1, 100, 5)
p2 = make_planet(0, 0, 2, 100, 5)
generate_coordinates(p1, r, theta1)
generate_coordinates(p2, r, theta2)

while not_valid(p1, p2) or distance(p1, p2) < minStartingDistance:
    r = rand_radius(minDistance, maxRadius)
    theta1 = rand_num(0, 360)
    if symmetryType == 1 and theta1 < 180:
        theta2 = theta1+180
    elif symmetryType == 1:
        theta2 = theta1-180
    else:
        theta2 = rand_num(0, 360)

    generate_coordinates(p1, r, theta1)
    generate_coordinates(p2, r, theta2)
planets.append(p1)
planets.append(p2)
planetsToGenerate -= 2

#makes the center neutral planets
if symmetryType == 1:
    noCenterNeutrals = 2*random.randint(0, maxCentral/2)
    thetaA = (theta1+theta2)/2
    thetaB = thetaA + 180
    for i in range(noCenterNeutrals/2):
        r = rand_radius(minDistance, maxRadius)
        num_ships = random.randint(minShips, maxShips)
        growth_rate = random.randint(minGrowth, maxGrowth)
        p1 = make_planet(0, 0, 0, num_ships, growth_rate)
        p2 = make_planet(0, 0, 0, num_ships, growth_rate)
        generate_coordinates(p1, r, thetaA)
        generate_coordinates(p2, r, thetaB)
        while not_valid(p1, p2):
            r = rand_radius(minDistance, maxRadius)
            generate_coordinates(p1, r, thetaA)
            generate_coordinates(p2, r, thetaB)
        planets.append(p1)
        planets.append(p2)
        planetsToGenerate -= 2
else:
    # must have an even number of planets left to generate after this
    minCentral = planetsToGenerate % 2
    noCenterNeutrals = random.randrange(minCentral, maxCentral+1, 2)
    theta = (theta1+theta2)/2
    if random.randint(0, 1) == 1:
        theta += 180
    for i in range(noCenterNeutrals):
        r = rand_radius(0, maxRadius)
        num_ships = random.randint(minShips, maxShips)
        growth_rate = random.randint(minGrowth, maxGrowth)
        p = make_planet(0, 0, 0, num_ships, growth_rate)
        generate_coordinates(p, r, theta)
        while not_valids(p):
            r = rand_radius(0, maxRadius)
            generate_coordinates(p, r, theta)
        planets.append(p)
        planetsToGenerate -= 1

#picks out the rest of the neutral planets
assert planetsToGenerate % 2 == 0, "Error: odd number of planets left to add"
for i in range(planetsToGenerate/2):
    r = rand_radius(minDistance, maxRadius)
    theta = rand_num(0, 360)
    if i == 0:
        num_ships = random.randint(minShips, 5*distance(p1, p2)-1)
    else:
        num_ships = random.randint(minShips, maxShips)
    growth_rate = random.randint(minGrowth, maxGrowth)
    p1 = make_planet(0, 0, 0, num_ships, growth_rate)
    p2 = make_planet(0, 0, 0, num_ships, growth_rate)
    generate_coordinates(p1, r, theta1+theta)
    generate_coordinates(p2, r, theta2 + symmetryType*theta)

    while not_valid(p1, p2):
        r = rand_radius(minDistance, maxRadius)
        theta = rand_num(0, 360)
        generate_coordinates(p1, r, theta1 + theta)
        generate_coordinates(p2, r, theta2 + symmetryType*theta)
    planets.append(p1)
    planets.append(p2)

translate_planets(planets)

for p in planets:
    print_planet(p)
