import random

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

planets = []
planets.append(make_planet(0, 0, 0, random.randint(0, 5), random.randint(1, 150)))
planets.append(make_planet(7, 7, 1, 5, 100))
planets.append(make_planet(-7, -7, 2, 5, 100))
for i in range(10):
  x = (random.random() * 2.0 - 1.0) * 10.0
  y = (random.random() * 2.0 - 1.0) * 10.0
  num_ships = random.randint(1, 150)
  growth_rate = random.randint(0, 5)
  planets.append(make_planet(x, y, 0, growth_rate, num_ships))
  planets.append(make_planet(-x, -y, 0, growth_rate, num_ships))
fix_coordinates(planets)
for p in planets:
  print_planet(p)
