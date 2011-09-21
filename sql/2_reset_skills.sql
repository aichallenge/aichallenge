-- this will reset the rankings so everyone starts at the initial mu and sigma
-- game counts will be reset as well to make the matchup generator start over
-- it will be used for finals

drop procedure if exists reset_skills;
delimiter $$
create procedure reset_skills()
begin

    -- delete player vs player game counts
    delete from opponents;

    -- delete player on map game counts
    -- delete from player_maps;

    -- reset submissions
    update submission
    set rank = null,
        rank_change = null,
        mu = 50.0,
        sigma = 16.6667,
        mu_change = null,
        sigma_change = null,
        min_game_id = null,
        max_game_id = null,
        game_count = 0;

    -- delete current matchups, even those in progress
    delete from matchup
    where worker_id >= 0 or worker_id is null;
    delete from matchup_player
    where not exists (
        select *
        from matchup
        where matchup.matchup_id = matchup_player.matchup_id
    );

end$$
delimiter ;

