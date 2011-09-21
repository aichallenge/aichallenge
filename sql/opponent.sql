drop procedure if exists opponent;
delimiter $$
create procedure opponent(in the_user_id int)
begin

-- get min and max players for matchmaking
select min(players) into @min_players from map;

select count(distinct s.user_id)
into @max_players
from submission s
inner join user u
    on u.user_id = s.user_id
left outer join matchup_player mp
    on mp.user_id = u.user_id
left outer join matchup m
    on m.matchup_id = mp.matchup_id
        and (m.worker_id > 0 or m.worker_id is null)
        and m.deleted = 0
where s.status = 40 and s.latest = 1
    and m.matchup_id is null;

-- skip entire process if less players are available than the smallest map
if @min_players <= @max_players then

    -- setup trueskill calc values
    set @init_mu = 50.0;
    set @init_beta = @init_mu / 6;
    set @twiceBetaSq = 2 * pow(@init_beta, 2);

    if the_user_id is null then
    
    -- Step 1: select the seed player
    select s.user_id, s.submission_id, s.mu, s.sigma
    into @seed_id, @submission_id, @mu, @sigma
    from submission s
    left outer join (
        select seed_id, max(matchup_id) as max_matchup_id
        from matchup
        where (worker_id >= 0 or worker_id is null)
            and deleted = 0
        group by seed_id
    ) m
        on s.user_id = m.seed_id
    where s.latest = 1 and s.status = 40
    -- this selects the user that has least recently played in any game
    -- and used them for the next seed player
    -- from both the game and matchup tables
    order by m.max_matchup_id asc,
             s.max_game_id asc,
             s.user_id asc
    limit 1;
    
    else
    
    select s.user_id, s.submission_id, s.mu, s.sigma
    into @seed_id, @submission_id, @mu, @sigma
    from submission s
    where s.user_id = the_user_id
        and s.latest = 1 and s.status = 40;
        
    end if;

    -- debug statement
    drop table if exists tmp_matchup;
    create table tmp_matchup
    select * from matchup;
    
    drop table if exists tmp_matchup_player;
    create table tmp_matchup_player
    select * from matchup_player;
    
    -- create matchup and add seed player
    -- worker_id of 0 prevents workers from pulling the task
    set @matchup_id = 0;
    insert into tmp_matchup (matchup_id, seed_id, worker_id)
    values (@matchup_id, @seed_id, 0);

    insert into tmp_matchup_player (matchup_id, user_id, submission_id, player_id, mu, sigma)
    values (@matchup_id, @seed_id, @submission_id, -1, @mu, @sigma);

    -- debug statement
    select @seed_id as seed_id, @submission_id as submission_id, @mu as mu, @sigma as sigma;

    -- Step 2: select the map
    -- TODO: improve distribution of games
    select m.map_id, m.players, m.max_turns
    into @map_id, @players, @max_turns
    from map m
    left outer join (
        select map_id, max(g.game_id) as max_game_id, count(*) as game_count
        from game g
        inner join game_player gp
            on g.game_id = gp.game_id
            and gp.user_id = @seed_id
        group by g.map_id
    ) games
        on m.map_id = games.map_id
    left outer join (
        select map_id, max(g.game_id) as max_all_game_id, count(*) as all_game_count
        from game g
        group by g.map_id
    ) all_games
        on m.map_id = all_games.map_id
    where m.players <= @max_players
        and m.priority >= 0
    -- sqrt is used to allow for a little leniency keeping the game count even across maps
    order by floor(sqrt(game_count)),
             floor(sqrt(all_game_count)),
             max_game_id,
             m.map_id
    limit 1;

    update tmp_matchup
    set map_id = @map_id,
        max_turns = @max_turns
    where matchup_id = @matchup_id;

    -- debug statement
    select * from map where map_id = @map_id;

    -- Step 3: select opponents 1 at a time
    set @cur_user_id = @seed_id;
    set @last_user_id = -1;
    set @player_count = 1;

    drop temporary table if exists temp_recent;
    create temporary table temp_recent (
        user_id int,
        recent_games int,
        primary key (user_id)
    );
    insert into temp_recent (user_id, recent_games)
        select user_id, count(*) as recent_games
        from game_player gp
        inner join game g
            on g.game_id = gp.game_id
        where g.timestamp > timestampadd(hour, -24, current_timestamp)
        group by user_id;


    while @player_count < @players do

            -- debug statement
            -- select list of opponents with match quality
            select s.user_id, s.submission_id, s.mu, s.sigma
            from (
                select @seq := @seq + 1 as seq, s.*
                from (
                    select s.user_id, s.submission_id, s.mu, s.sigma
                        -- trueskill match quality for 2 players
                        ,@c := (@twiceBetaSq + pow(p.sigma,2) + pow(s.sigma,2)) as c,
                        @match_quality := exp(sum(ln(
                            sqrt(@twiceBetaSq / @c) * exp(-(pow(p.mu - s.mu, 2) / (2 * @c)))
                        ))) as match_quality
                    from
                    submission p, -- current players in match
                    submission s  -- possible next players
                    -- join with all players in current matchup to average match quality
                    where p.submission_id in (
                        select submission_id
                        from tmp_matchup_player
                        where matchup_id = @matchup_id
                    )
                    -- exclude players currently in a matchup
                    and not exists (
                        select *
                        from tmp_matchup m
                        inner join tmp_matchup_player mp
                            on mp.matchup_id = m.matchup_id
                        where mp.user_id = s.user_id
                        and (m.worker_id >= 0 or m.worker_id is null)
                        and m.deleted = 0
                    )
                    and s.latest = 1 and s.status = 40
                    group by s.user_id, s.submission_id, s.mu, s.sigma
                    -- power curve approximation, pulls in some lower quality bots occasionally
                    order by @match_quality * rand() desc
                ) s,
                (select @seq := 0) seq
            ) s
            inner join temp_recent r
                on r.user_id = s.user_id
            -- join in user to user game counts to provide round-robin like logic
            left outer join opponents o
                on o.user_id = @seed_id and o.opponent_id = s.user_id,
            -- get count of all active submissions to limit to top 10%
            (
                select count(*) as submission_count
                from submission
                where latest = 1 and status = 40
            ) s_count
            where s.seq < s_count.submission_count * 10/100 or s.seq <= @min_players * 2
            order by o.game_count,
                r.recent_games,
                s.match_quality desc;
            
            -- select list of opponents with match quality
            select s.user_id, s.submission_id, s.mu, s.sigma
            into @last_user_id, @last_submission_id, @last_mu, @last_sigma
            from (
                select @seq := @seq + 1 as seq, s.*
                from (
                    select s.user_id, s.submission_id, s.mu, s.sigma
                        -- trueskill match quality for 2 players
                        ,@c := (@twiceBetaSq + pow(p.sigma,2) + pow(s.sigma,2)) as c,
                        @match_quality := exp(sum(ln(
                            sqrt(@twiceBetaSq / @c) * exp(-(pow(p.mu - s.mu, 2) / (2 * @c)))
                        ))) as match_quality
                    from
                    submission p, -- current players in match
                    submission s  -- possible next players
                    -- join with all players in current matchup to average match quality
                    where p.submission_id in (
                        select submission_id
                        from tmp_matchup_player
                        where matchup_id = @matchup_id
                    )
                    -- exclude players currently in a matchup
                    and not exists (
                        select *
                        from tmp_matchup m
                        inner join tmp_matchup_player mp
                            on mp.matchup_id = m.matchup_id
                        where mp.user_id = s.user_id
                        and (m.worker_id >= 0 or m.worker_id is null)
                        and m.deleted = 0
                    )
                    and s.latest = 1 and s.status = 40
                    group by s.user_id, s.submission_id, s.mu, s.sigma
                    -- power curve approximation, pulls in some lower quality bots occasionally
                    order by @match_quality * rand() desc
                ) s,
                (select @seq := 0) seq
            ) s
            inner join temp_recent r
                on r.user_id = s.user_id
            -- join in user to user game counts to provide round-robin like logic
            left outer join opponents o
                on o.user_id = @seed_id and o.opponent_id = s.user_id,
            -- get count of all active submissions to limit to top 10%
            (
                select count(*) as submission_count
                from submission
                where latest = 1 and status = 40
            ) s_count
            where s.seq < s_count.submission_count * 10/100 or s.seq <= @min_players * 2
            order by o.game_count,
                r.recent_games,
                s.match_quality desc
            limit 1;
                
            -- debug statement
            select @last_user_id as user_id, @last_submission_id as submission_id, @last_mu as mu, @last_sigma as sigma;

            -- add new player to matchup
            insert into tmp_matchup_player (matchup_id, user_id, submission_id, player_id, mu, sigma)
            values (@matchup_id, @last_user_id, @last_submission_id, -1, @last_mu, @last_sigma);
            set @player_count = @player_count + 1;
            set @cur_user_id = @last_user_id;
            
    end while;

    -- Step 4: put players into map positions
    update tmp_matchup_player
    inner join (
        select @position := (@position + 1) as position,
            m.user_id
        from (
            select mp.*
            from tmp_matchup_player mp
            where matchup_id = @matchup_id
            order by rand()
        ) m,
        (select @position := -1) p
    ) m2
        on tmp_matchup_player.user_id = m2.user_id
    set player_id = m2.position
    where matchup_id = @matchup_id;

    -- debug statement
    select * from matchup m inner join matchup_player mp on mp.matchup_id = m.matchup_id where m.matchup_id = @matchup_id;

    -- turn matchup on
    update tmp_matchup
    set worker_id = null
    where matchup_id = @matchup_id;

    -- return new matchup id
    select @matchup_id as matchup_id;

else

    -- debug statement
    select "matchup skipped because available players is less than smallest map";
    
end if;

end$$
delimiter ;
