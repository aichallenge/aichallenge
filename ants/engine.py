#!/usr/bin/env python
from sandbox import Sandbox
from ants import Ants
import time
import traceback

def save_image(game, turn, anno="", player=None):
    img = game.render(player)
    scale = 4
    new_size = (img.size[0] * scale, img.size[1] * scale)
    img = img.resize(new_size)
    img.save("playback/%s_%s.png" % (anno, str(turn).zfill(5)))

def run_game(game, botcmds, timeoutms, loadtimeoutms, num_turns=1000,
             output_file="testout.txt", verbose=False, serial=False):
    try:
        f = open('bot1.txt', 'w')
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

                # send game state to each player
                for b, bot in enumerate(bots):
                    if game.is_alive(b):
                        if turn == 0:
                            bot.write(game.get_player_start(b) + 'ready\n')
                            if b == 0:
                                f.write(game.get_player_start(b))
                                f.flush()
                        else:
                            bot.write(game.get_player_state(b) + 'go\n')
                            if b == 0:
                                f.write(game.get_player_state(b))
                                f.flush()
                if turn > 0:
                    game.start_turn()

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
                        line = line.strip()
                        if line.lower() == 'go':
                            bot_finished[b] = True
                        else:
                            bot_moves[b] += line + '\n'

                # process all moves
                if turn > 0 and not game.game_over():
                    game.do_all_moves(bot_moves)
                    game.finish_turn()

            except:
                traceback.print_exc()
                print("Got an error running the bots.")
                raise

            if output_file:
                of.write(game.get_state())
                of.flush()
            for i in range(len(bots)):
                save_image(game, turn, 'player%s' % i, i)
            save_image(game, turn, 'frame')
            #print(game.get_state())

            if verbose:
                stats = game.get_stats()
                s = 'turn %4d stats: '
                for key, values in stats:
                    s += '%s: %s' % (key, values)
                sys.stderr.write("\r%-50s" % s)

            alive = [game.is_alive(b) for b in range(len(bots))]
            #print('alive %s' % alive)
            if sum(alive) <= 1:
                break

        game.finish_game()
        #print(game.get_state())

    finally:
        for bot in bots:
            if bot.is_alive:
                bot.kill()
        if output_file:
            of.close()
    return "Game Over, %s" % game.get_scores()
    f.close()
