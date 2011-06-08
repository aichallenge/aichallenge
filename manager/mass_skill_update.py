#!/usr/bin/python
# Query the database for all games without player trueskill updates done and
# attempt to fill them in
import manager
import MySQLdb

_unfilled_game_query = """
select game_id from game_player where mu_after is null
    group by game_id;
"""

def main():
    connection = manager.get_connection()
    cursor = connection.cursor(MySQLdb.cursors.DictCursor)

    cursor.execute(_unfilled_game_query)
    results = cursor.fetchall()
    for row in results:
        manager.update_trueskill(row['game_id'])

if __name__ == "__main__":
    main()

