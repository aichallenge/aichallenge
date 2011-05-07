#!/usr/bin/python
from __future__ import print_function
import MySQLdb
from server_info import server_info
from sql import sql
from trueskill import trueskill
import argparse
import sys

class Player(object):
    def __init__(self, name, skill, rank):
        self.name = name
        self.old_skill = skill
        self.skill = skill
        self.rank = rank
    def __str__(self):
        return ('id=%5d rank=%1d\n\t   mu=%8.5f->%8.5f,\n\tsigma=%8.5f->%8.5f' %
                (self.name, self.rank, self.skill[0], self.skill[0], self.old_skill[1], self.skill[1]))
        
def update_trueskill(game_id):
    connection = MySQLdb.connect(host = server_info["db_host"],
                                 user = server_info["db_username"],
                                 passwd = server_info["db_password"],
                                 db = server_info["db_name"])
    cursor = connection.cursor(MySQLdb.cursors.DictCursor)

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
    print("After:")
    for player in players:
        print(player)
        cursor.execute(sql['update_game_player_trueskill'], (player.old_skill[0], player.old_skill[1], player.skill[0], player.skill[1], game_id, player.name))
    connection.commit()
    cursor.execute(sql['update_submission_trueskill'], game_id)
    connection.commit()
    return True

def reset_submissions():
    connection = MySQLdb.connect(host = server_info["db_host"],
                                 user = server_info["db_username"],
                                 passwd = server_info["db_password"],
                                 db = server_info["db_name"])
    cursor = connection.cursor(MySQLdb.cursors.DictCursor)
    cursor.execute('update submission set status = 20 where latest = 1')
    connection.commit()
    
def main():
    reset_submissions()
    
if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument("-g", "--game_id")
    args = parser.parse_args()
    
    if args.game_id:
        if update_trueskill(args.game_id):
            sys.exit(0)
        else:
            sys.exit(-1)