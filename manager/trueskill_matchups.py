#!/usr/bin/python
''' TrueSkill matchmaking for the Google AI Challenge '''

import MySQLdb
from server_info import server_info

SQL = {
	# No arguments.
	"seed_user_id": """
SELECT u.user_id AS seed_user_id
FROM user AS u
     JOIN submission AS s
       ON u.user_id = s.user_id
     LEFT JOIN game_player AS gp
            ON s.submission_id = gp.submission_id
GROUP BY seed_user_id
ORDER BY CASE WHEN MAX(gp.game_id) IS NULL THEN 1 ELSE 2 END ASC,
         MAX(gp.game_id) ASC, u.user_id ASC
LIMIT 1
;
""",
	
	# Arguments: user_id (of "seed" user)
	"best_player_count": """
SELECT m.players as best_player_count
FROM map AS m
     LEFT JOIN (SELECT g.map_id AS map_id,
                       gp.user_id AS user_id,
                       g.seed_id AS seed_id,
                       g.game_id AS game_id
                FROM game AS g
                     JOIN game_player AS gp
                       ON g.game_id = gp.game_id
                WHERE gp.user_id = %(seed_user_id)s
               ) AS j1
            ON m.map_id = j1.map_id
WHERE m.players <= (SELECT COUNT(DISTINCT u.user_id)
                    FROM user AS u
                         JOIN submission AS s
                         ON u.user_id = s.user_id
                   )
GROUP BY best_player_count
ORDER BY CASE
          WHEN COUNT(j1.user_id) = 0 THEN 1
          WHEN COUNT(CASE
                      WHEN j1.seed_id = j1.user_id
                      THEN j1.user_id
                     END) = 0
               THEN 2
          ELSE 3
         END ASC,
         MAX(j1.game_id) ASC, MIN(m.map_id) ASC
LIMIT 1
;
""",
	
	# Arguments: user_id (of "seed" user), players (count on map)
	"best_map_id": """
SELECT m.map_id as map_id
FROM submission AS s
     JOIN game_player AS gp
     ON s.submission_id = gp.submission_id
     JOIN game AS g
     ON gp.game_id = g.game_id
     RIGHT JOIN map AS m
     ON g.map_id = m.map_id
WHERE (s.user_id IS NULL OR s.user_id = %(seed_user_id)s)
  AND m.players = %(players)s
GROUP BY m.map_id
ORDER BY COUNT(s.user_id) ASC, MAX(g.timestamp) ASC
LIMIT 1
;
""",
	
	# Arguments: user_ids (Set of players already chosen)
	"create_opp_data": """
CREATE TEMPORARY TABLE temp_opp_data
SELECT s.user_id AS user_id, s.submission_id AS submission_id,
       s.sigma AS sigma, s.mu AS mu
FROM submission AS s
WHERE s.user_id IN %(current_players)s
;
""",
	
	"create_opp_data_2": """
CREATE TEMPORARY TABLE temp_opp_data_2 LIKE temp_opp_data
;
""",
	
	"copy_opp_data": """
INSERT INTO temp_opp_data_2
SELECT *
FROM temp_opp_data
;
""",
	
	# Arguments: twice_beta_squared (TrueSkill parameter),
	#            players (count on map),
	#            y (number of games before replaying at same player count),
	#            map_id (which map the game will be played)
	#            z (number of games before replaying the same map),
	#            x (number of games before replaying the same users)
	"next_player": """
SELECT ss.user_id,
       EXP(SUM(LOG(draw_probability))) AS quality
FROM
(
SELECT s.user_id AS user_id,
       @c := (@twiceBetaSq + od.sigma ^ 2 + s.sigma ^ 2) AS c,
       SQRT(@twiceBetaSq / @c) * EXP(-((od.mu - s.mu) ^ 2 / (2 * @c)))
        AS draw_probability
FROM submission AS s,
     (SELECT @twiceBetaSq := %(2B2)s) AS v1,
     temp_opp_data AS od
WHERE %(players)s NOT IN (
                         SELECT m.players AS players
                         FROM game AS g
                              JOIN map AS m
                              ON g.map_id = m.map_id
                         WHERE g.game_id IN (
                                            SELECT game_id
                                            FROM
                                            (
                                            SELECT rg.game_id AS game_id,
                                                   (@rgn := @rgn + 1) AS rgn
                                            FROM submission AS rs
                                                 JOIN game_player AS rgp
                                                 ON rs.submission_id = rgp.submission_id
                                                 JOIN game AS rg
                                                 ON rgp.game_id = rg.game_id,
                                                 (SELECT @rgn := 0) AS v1
                                            WHERE rs.user_id = user_id
                                              AND @rgn <= %(limit_players)s
                                            ORDER BY rg.timestamp DESC
                                            ) AS limity
                                            )
                         )
  AND %(map)s NOT IN (
                     SELECT m.map_id AS map_id
                     FROM game AS g
                          JOIN map AS m
                          ON g.map_id = m.map_id
                     WHERE g.game_id IN (
                                        SELECT game_id
                                        FROM
                                        (
                                        SELECT rg.game_id AS game_id,
                                               (@rgn := @rgn + 1) AS rgn
                                        FROM submission AS rs
                                             JOIN game_player AS rgp
                                             ON rs.submission_id = rgp.submission_id
                                             JOIN game AS rg
                                             ON rgp.game_id = rg.game_id,
                                             (SELECT @rgn := 0) AS v1
                                        WHERE rs.user_id = user_id
                                          AND @rgn <= %(limit_map)s
                                        ORDER BY rg.timestamp DESC
                                        ) AS limitz
                                        )
                     )
  AND NOT EXISTS (
                 SELECT o.user_id AS user_id
                 FROM game AS g
                      JOIN game_player AS gp
                      ON g.game_id = gp.game_id
                      JOIN submission AS os
                      ON gp.submission_id = os.submission_id
                      RIGHT JOIN temp_opp_data_2 AS o
                      ON os.user_id = o.user_id
                 WHERE o.user_id = s.user_id
                    OR g.game_id IN (
                                    SELECT game_id
                                    FROM
                                    (
                                    SELECT rg.game_id AS game_id,
                                           (@rgn := @rgn + 1) AS rgn
                                    FROM submission AS rs
                                         JOIN game_player AS rgp
                                         ON rs.submission_id = rgp.submission_id
                                         JOIN game AS rg
                                         ON rgp.game_id = rg.game_id,
                                         (SELECT @rgn := 0) AS v1
                                    WHERE rs.user_id = user_id
                                      AND @rgn <= %(limit_pair)s
                                    ORDER BY rg.timestamp DESC
                                    ) AS limitx
                                    )
                 )
) AS ss
GROUP BY ss.user_id
ORDER BY quality DESC
LIMIT 1
;
""",
	
	# No arguments.
	"drop_opp_data_2": """
DROP TEMPORARY TABLE temp_opp_data_2
;
""",
	
	# No arguments.
	"drop_opp_data": """
DROP TEMPORARY TABLE temp_opp_data
;
""",
}

BETA = 8.333 # INITIAL_SIGMA / 2 (example only)
X = 1
Y = 1
Z = 1

def set_to_db(c, s):
	''' Work around broken set substitution '''
	return '(' + ','.join(map(c.literal, s)) + ')'

def main():
	''' Example run, does a single matchmaking session from the database. '''
	connection = MySQLdb.connect(host = server_info["db_host"],
	                             user = server_info["db_username"],
	                             passwd = server_info["db_password"],
	                             db = server_info["db_name"])
	cursor = connection.cursor()
	
	# Find a seed player
	cursor.execute(SQL["seed_user_id"])
	seed_user_id = cursor.fetchone()[0]
	print "Seed User Id: ", seed_user_id
	
	# Determine how many players in the game
	cursor.execute(SQL["best_player_count"], {
		"seed_user_id": seed_user_id,
	})
	players = cursor.fetchone()[0]
	print "Players: ", players
	
	# Determine the map for the game
	cursor.execute(SQL["best_map_id"], {
		"seed_user_id": seed_user_id,
		"players": players,
	})
	map_id = cursor.fetchone()[0]
	print "Map Id: ", map_id
	
	# Match rest of players one at a time
	current_players = set([seed_user_id])
	while (players > len(current_players)):
		# Create and populate the temporary tables
		cursor.execute(SQL["create_opp_data"] % {
			"current_players": set_to_db(connection, current_players),
		})
		cursor.execute(SQL["create_opp_data_2"])
		cursor.execute(SQL["copy_opp_data"])
		
		# Determine the next player to add to the game
		cursor.execute(SQL["next_player"], {
			"2B2": 2 * BETA ** 2,
			"limit_pair": X,
			"players": players,
			"limit_players": Y,
			"map": map_id,
			"limit_map": Z,
		})
		current_players.add(cursor.fetchone()[0])
		print "Current Players: ", current_players
		
		# Drop our temporary tables
		cursor.execute(SQL["drop_opp_data_2"])
		cursor.execute(SQL["drop_opp_data"])

if __name__ == "__main__":
	main()
