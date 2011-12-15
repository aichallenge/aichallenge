#!/usr/bin/python

import sys
import time
import random

import MySQLdb
import MySQLdb.cursors

from server_info import server_info

def log(msg):
    timestamp = time.asctime()
    print "%s: %s" % (timestamp, msg)

class Matchup:
    def __init__(self, game_map):
        self.map = game_map
        self.players = []

    def commit(self, cursor):
        log("Match on map %s" % (self.map["filename"],))
        log("with players: %s" % ([p["user_id"] for p in self.players],))
        cursor.execute("""insert matchup (seed_id, worker_id, map_id, max_turns)
                values (%s, 0, %s, %s)""",
                (self.players[0]["user_id"], self.map["map_id"],
                    self.map["max_turns"])
                )
        matchup_id = cursor.lastrowid
        for num, player in enumerate(self.players):
            cursor.execute("""insert matchup_player
                (matchup_id, user_id, submission_id, player_id, mu, sigma)
                values (%s, %s, %s, %s, %s, %s)""",
                (matchup_id, player["user_id"], player["submission_id"], num,
                    player["mu"], player["sigma"])
                )
        cursor.execute("""update matchup set worker_id = null
                where matchup_id = %s""", (matchup_id,))
        log("inserted as match %d" %(matchup_id,))


def main(rounds = 1):
    connection = MySQLdb.connect(host = server_info["db_host"],
                                 user = server_info["db_username"],
                                 passwd = server_info["db_password"],
                                 db = server_info["db_name"],
                                 cursorclass = MySQLdb.cursors.DictCursor)
    cursor = connection.cursor()

    cursor.execute("select * from map where priority > 0")
    maps = cursor.fetchall()

    cursor.execute("select * from submission where latest = 1")
    players = list(cursor.fetchall())
    random.shuffle(players)

    cur_round = 1
    map_ix = 0
    player_ix = 0
    pairings = 0
    while cur_round <= rounds:
        match = Matchup(maps[map_ix])
        map_ix = (map_ix + 1) % len(maps)
        for player_num in range(match.map["players"]):
            next_player = players[player_ix]
            while next_player in match.players:
                # Should only happen when a game wraps around the end of the 
                # player list
                assert player_ix < player_num, "Duplicate player later in list"
                rnd_ix = random.randint(player_ix + 1, len(players) - 1)
                players[player_ix], players[rnd_ix] = players[rnd_ix], players[player_ix]
                next_player = players[player_ix]
            match.players.append(next_player)
            player_ix = (player_ix + 1) % len(players)
            if player_ix == 0:
                cur_round += 1
                random.shuffle(players)
        match.commit(cursor)
        pairings += 1

    log("Paired %d rounds with %d matches" % (rounds, pairings))


if __name__ == "__main__":
    rounds = 1
    if len(sys.argv) > 1:
        rounds = int(sys.argv[1])
    main(rounds)

