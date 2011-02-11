#!/usr/bin/env python
from sandbox import Sandbox
from ants import Ants
import time

import copy

player_count = 4

def switch_players(m, a, b):
    m = m.clone()
    for x in range(m.width):
        for y in range(m.height):
            if m.map[x][y] == a:
                m.map[x][y] = b
            elif m.map[x][y] == b:
                m.map[x][y] = a
    for key in m.ant_list.keys():
        if m.ant_list[key] == a:
            m.ant_list[key] = b
        elif m.ant_list[key] == b:
            m.ant_list[key] = a
    return m

def save_image(game, turn, anno="", player=None):
    img = game.render(player)
    scale = 4
    new_size = (img.size[0] * scale, img.size[1] * scale)
    img = img.resize(new_size)
    img.save("playback/frame" + str(turn).zfill(5) + anno + ".png")

def play_game(map_filename):
    m = AntMap(map_filename)
    players = [HunterBot() for i in range(m.num_players)]
    perspective = [[0 for i in range(m.num_players)] for i in range(m.num_players)]
    initial_food_density = 0.01
    food_amount = int(initial_food_density * m.land_area)
    m.do_food(food_amount)
    turn_count = 1
    score = [0 for i in range(len(players))]
    while not m.game_over() and turn_count < 1000:
        print "turn:", turn_count
        save_image(m, turn_count, 'start')
        for i, player in enumerate(players):
            player_number = i + 1
            reflected_map = switch_players(m, player_number, 1)
            orders = player.do_turn(reflected_map)
            m.do_orders(player_number, orders)
            save_image(m, turn_count, 'player' + str(i+1))
        m.resolve_orders()
        save_image(m, turn_count, 'move')
        m.do_death()
        save_image(m, turn_count, 'death')
        m.do_birth()
        save_image(m, turn_count, 'birth')
        m.do_food()
        save_image(m, turn_count, 'food')
        turn_count += 1
        turn_score = m.do_score()
        for i in range(len(players)+1):
            score[i] += turn_score[i]
    save_image(m, turn_count, 'start')
    print score

def run_game(game, botcmds, timeoutms, loadtimeoutms, num_turns=1000,
             output_file="testout.txt", verbose=False, serial=False):
    try:
        # create bot sandboxes
        bots = [Sandbox(*bot) for bot in botcmds]
        for b, bot in enumerate(bots):
            if not bot.is_alive:
                print('bot %s did not start' % botcmds[b])
                game.kill_player(b)

        if output_file:
            of = open(output_file, "w")
            of.write(game.get_state())
            of.flush()

        print('running for %s turns' % num_turns)
        for turn in range(num_turns+1):
            print('turn %s' % turn)
            try:
                if turn == 1:
                    game.start_game()
                game.start_turn()

                # send game state to each player
                for b, bot in enumerate(bots):
                    if game.is_alive(b):
                        if turn == 0:
                            bot.write(game.get_player_start(b) + 'ready\n')
                        else:
                            bot.write(game.get_player_state(b) + 'go\n')

                game.resolve_orders()
                # get moves from each player
                if turn == 0:
                    time_limit = float(loadtimeoutms) / 1000
                else:
                    time_limit = float(timeoutms) / 1000
                start_time = time.time()
                bot_finished = [not game.is_alive(b) for b in range(len(bots))]
                bot_moves = ['' for b in bots]

                # loop until received all bots send moves or are dead
                #   or when time is up
                while (sum(bot_finished) < len(bot_finished) and
                        time.time() - start_time < time_limit):
                    time.sleep(0.1)
                    for b, bot in enumerate(bots):
                        if bot_finished[b]:
                            continue # already got bot moves
                        if not bot.is_alive:
                            print('bot died')
                            bot_finished[b] = True
                            game.kill_player(b)
                            continue # bot is dead
                        line = bot.read_line()
                        if line is None:
                            continue
                        line = line.strip().lower()
                        if line == 'go':
                            bot_finished[b] = True
                        else:
                            bot_moves[b] += line + '\n'

                # process all moves
                if turn > 0:
                    game.do_all_moves(bot_moves)
                    game.finish_turn()

            except:
                print("Got an error running the bots.")
                raise

            if output_file:
                of.write(game.get_state())
                of.flush()
            for i in range(len(bots)):
                save_image(game, turn, 'player%s' % i, i)
            save_image(game, turn, 'start')
            #print(game.get_state())

            if verbose:
                stats = game.get_stats()
                s = 'turn %4d stats: '
                for key, values in stats:
                    s += '%s: %s' % (key, values)
                sys.stderr.write("\r%-50s" % s)

            alive = [game.is_alive(b) for b in range(len(bots))]
            #print('alive %s' % alive)
            if sum(alive) == 0:
                break

        game.finish_game()
        for bot in bots:
            if bot.is_alive:
                bot.kill()
        print(game.get_state())
        return "Game Over, %s" % game.get_scores()

    finally:
        if output_file:
            of.close()

if __name__ == '__main__':
    a = Ants('maps/random4.txt')
    print(a.get_player_state(0))
    print(a.get_player_state(1))
    print(a.get_player_state(2))
    print(a.get_player_state(3))
