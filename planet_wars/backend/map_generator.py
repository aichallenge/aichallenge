import math
import random

planets = []

def make_planet(x, y, owner, growth_rate, num_ships):
  return {"x" : x, "y" : y, "owner" : owner, "growth_rate" : growth_rate, "num_ships" : num_ships}

def print_planet(p):
  print "P " + str(p["x"]) + " " + str(p["y"]) + " " + str(p["owner"]) + " " + str(p["num_ships"]) + " " + str(p["growth_rate"])

def translate_planets(planets, dx, dy):
  for p in planets:
    p["x"] += dx
    p["y"] += dy

def min_coords(planets):
  return min([p["x"] for p in planets]), min([p["y"] for p in planets])

def fix_coordinates(planets):
  min_x, min_y = min_coords(planets)
  translate_planets(planets, -min_x, -min_y)

def rand_coord():
  map_radius = 12
  return (random.random() * 2.0 - 1.0) * map_radius

def too_close(x, y, growth_rate):
  multiplier = 0.4
  for p in planets:
    dx = x - p["x"]
    dy = y - p["y"]
    dist = math.sqrt(dx * dx + dy * dy)
    threshold = \
      multiplier * (math.sqrt(growth_rate) + math.sqrt(p["growth_rate"]))
    if dist < threshold:
      return True
  return False

planets.append(make_planet(0, 0, 0, random.randint(0, 5), random.randint(1, 150)))
x = rand_coord()
y = rand_coord()
planets.append(make_planet(x, y, 1, 5, 100))
planets.append(make_planet(-x, -y, 2, 5, 100))
for i in range(10):
  done = False
  while not done:
    x = rand_coord()
    y = rand_coord()
    num_ships = random.randint(1, 90)
    growth_rate = random.randint(1, 5)
    if too_close(x, y, growth_rate):
      continue
    planets.append(make_planet(x, y, 0, growth_rate, num_ships))
    planets.append(make_planet(-x, -y, 0, growth_rate, num_ships))
    done = True
fix_coordinates(planets)
for p in planets:
  print_planet(p)
