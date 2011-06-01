#!/usr/bin/env python
from __future__ import print_function
import MySQLdb
from server_info import server_info
from sql import sql
from trueskill import trueskill
import argparse
import sys
import time
import logging
import logging.handlers
import os
import traceback

use_log = True

# Set up logging
log = logging.getLogger('manager')
log.setLevel(logging.INFO)

formatter = logging.Formatter("%(asctime)s - " + str(os.getpid()) +
                              " - %(levelname)s - %(message)s")

log_file = os.path.join(server_info['logs_path'], 'manager.log')
try:
    handler = logging.handlers.RotatingFileHandler(log_file,
                                                   maxBytes=1000000,
                                                   backupCount=5)
    handler.setLevel(logging.INFO)
    handler.setFormatter(formatter)
    log.addHandler(handler)
except IOError:
    # ignore errors about file permissions
    pass

handler2 = logging.StreamHandler()
handler2.setLevel(logging.INFO)
handler2.setFormatter(formatter)
log.addHandler(handler2)

class Player(object):
    def __init__(self, name, skill, rank):
        self.name = name
        self.old_skill = skill
        self.skill = skill
        self.rank = rank
    def __str__(self):
        return ('id=%5d rank=%1d\n\t   mu=%8.5f->%8.5f,\n\tsigma=%8.5f->%8.5f' %
                (self.name, self.rank, self.skill[0], self.skill[0], self.old_skill[1], self.skill[1]))

connection = None
def get_connection():
    global connection
    if connection == None:
        connection = MySQLdb.connect(host = server_info["db_host"],
                                     user = server_info["db_username"],
                                     passwd = server_info["db_password"],
                                     db = server_info["db_name"])
    return connection

def update_trueskill(game_id):
    log.info("Updating TrueSkill for game {0}".format(game_id))
    conn = get_connection()
    cursor = conn.cursor(MySQLdb.cursors.DictCursor)

    # get list of players and their mu/sigma values from the database

    players = []
    cursor.execute(sql['select_game_players'], game_id)
    results = cursor.fetchall()
    for row in results:
        player = Player(row['submission_id'], (row['mu'], row['sigma']), row['game_rank'])
        players.append(player)
        # check to ensure all rows have null _after values
        if row['mu_after'] != None:
            print("game already has values!")
            return False
    trueskill.AdjustPlayers(players)
    for player in players:
        log.debug(player)
        cursor.execute(sql['update_game_player_trueskill'], (player.old_skill[0], player.old_skill[1], player.skill[0], player.skill[1], game_id, player.name))
    conn.commit()
    cursor.execute(sql['update_submission_trueskill'], game_id)
    conn.commit()
    return True

def update_leaderboard(wait_time):
    conn = get_connection()
    cursor = conn.cursor(MySQLdb.cursors.DictCursor)
    while True:
        try:
            for s in range(wait_time):
                # allow for a [Ctrl]+C during the sleep cycle
                time.sleep(1)
            if use_log:
                log.info("Updating leaderboard and adding some sigma")
            cursor.execute("call generate_leaderboard;")
            if wait_time == 0:
                break
        except KeyboardInterrupt:
            break
        except:
            # log error
            log.error(traceback.format_exc())
            break
    cursor.close()
    conn.close()

def reset_submissions(status):
    log.info("Resetting all latest submissions to status {0}".format(status))
    conn = get_connection()
    cursor = conn.cursor(MySQLdb.cursors.DictCursor)
    cursor.execute('update submission set status = 20 where latest = 1')
    conn.commit()

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("-g", "--game_id", type=int,
                        help="game_id to update")
    parser.add_argument("-l", "--leaderboard", type=int, default=None,
                        help="produce a new leaderboard every X seconds")
    parser.add_argument('-r', '--reset', type=int,
                        help="reset submissions to status")
    parser.add_argument('--debug', default=False,
                        action='store_true',
                        help="Set the log level to debug")
    args = parser.parse_args()

    if args.debug:
        log.setLevel(logging.DEBUG)

    if args.game_id:
        if update_trueskill(args.game_id):
            sys.exit(0)
        else:
            sys.exit(-1)
    elif args.leaderboard != None:
        update_leaderboard(args.leaderboard)
    elif args.reset:
        reset_submissions(args.reset)
    else:
        parser.print_usage()

if __name__ == '__main__':
    main()
