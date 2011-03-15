import MySQLdb
from server_info import server_info
import sys

def main():
    connection = MySQLdb.connect(host = server_info["db_host"],
                                 user = server_info["db_username"],
                                 passwd = server_info["db_password"],
                                 db = server_info["db_name"])
    cursor = connection.cursor(MySQLdb.cursors.DictCursor)
    
    cursor.execute("insert into leaderboard (timestamp, algorithm_name, calculation_time) values (CURRENT_TIMESTAMP, 'TrueSkill', 0)")
    leaderboard_id = connection.insert_id()
    
    min_elo = 1
    player_results = []
    f = open("ratings.txt", "r")
    f.readline()
    for line in f:
      csv = ','.join(line.strip().split())
      tokens = csv.split(",")
      if len(tokens) != 11:
        continue
      (rank, user_id, username, submission_id, elo, plus_bound, minus_bound, \
        num_games, score, oppo, draws) = tokens
      elo = int(elo)
      if min_elo > elo:
        min_elo = elo
      player_results.append({'id':int(submission_id), 'rank':int(rank), 'elo':elo})
    f.close()
    
    cursor.execute("""
      INSERT INTO leaderboards
      (timestamp,algorithm_name,calculation_time)
      VALUES
      (CURRENT_TIMESTAMP,'Bayeselo',""" + str(elapsed_time) + """)
    """)
    leaderboard_id = connection.insert_id()
    
    game_values = []
    for player in player_results:
      values = "(%d,%d,%d,0,0,0,%d)" % (
        leaderboard_id, player['id'], player['rank'], player['elo']-min_elo)
      game_values.append(values)
    
    if game_values:
      cursor.execute("""
        INSERT INTO rankings
        (leaderboard_id,submission_id,rank,wins,losses,draws,score)
        VALUES %s""" % (",".join(game_values),))
    cursor.execute("UPDATE leaderboards SET complete=1 WHERE leaderboard_id=%d" %
            (leaderboard_id,))
    cursor.close()
    connection.close()

if __name__ == '__main__':
    main()