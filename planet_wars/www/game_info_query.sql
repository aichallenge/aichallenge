SELECT
  g.*,
  u1.username AS player_one,
  u2.username AS player_two,
  g.player_one as player_one_id,
  g.player_two as player_two_id
FROM games g
INNER JOIN submissions s1 ON g.player_one = s1.submission_id
INNER JOIN users u1 ON s1.user_id = u1.user_id
INNER JOIN submissions s2 ON g.player_two = s2.submission_id
INNER JOIN users u2 ON s2.user_id = u2.user_id
WHERE g.game_id = 