import copy

from map import *
from random_bot import *
from hunter_bot import *

def switch_players(m, a, b):
    m = copy.deepcopy(m)
    for ant in m.ants:
        if ant.owner == a:
            ant.owner = b
        elif ant.owner == b:
            ant.owner = a
    return m

def save_image(map, turn):
    img = map.render()
    scale = 4
    new_size = (img.size[0] * scale, img.size[1] * scale)
    img = img.resize(new_size)
    img.save("playback/frame" + str(turn).zfill(5) + ".png")

def play_game(map_filename, players):
    m = Map(map_filename)
    initial_food_density = 0.01
    food_amount = int(initial_food_density * m.land_area)
    m.randomly_place_food(food_amount)
    turn_count = 1
    while not m.game_over() and turn_count < 1000:
        print "turn:", turn_count
        save_image(m, turn_count)
        for i, player in enumerate(players):
            player_number = i + 1
            reflected_map = switch_players(m, player_number, 1)
            orders = player.do_turn(reflected_map)
            for order in orders:
                m.do_order(order)
        m.do_turn()
        turn_count += 1
    save_image(m, turn_count)

#players = [RandomBot(), RandomBot(), RandomBot(), HunterBot()]
players = [HunterBot(), HunterBot(), HunterBot(), HunterBot()]
play_game("simple_map_large.png", players)
