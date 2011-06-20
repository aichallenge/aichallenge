select max(game_id) into @max_game_id from game;

select user_id,
        max(game_id) as max_game_id,
        @max_game_id - max(game_id) as game_delay,
        count(*) as games_played
from game_player
where game_id > @max_game_id - 1000
group by user_id
order by game_delay desc;

select user_id,
        max(game_id) as max_game_id,
        @max_game_id - max(game_id) as game_delay,
        count(*) as games_played
from game_player
where game_id > @max_game_id - 1000
group by user_id
order by games_played;