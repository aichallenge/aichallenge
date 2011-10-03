drop procedure if exists generate_matchup;
delimiter $$
create procedure generate_matchup()
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

    -- create matchup and add seed player
    -- worker_id of 0 prevents workers from pulling the task
    insert into matchup (seed_id, worker_id)
    values (@seed_id, 0);
    set @matchup_id = last_insert_id();

    insert into matchup_player (matchup_id, user_id, submission_id, player_id, mu, sigma)
    values (@matchup_id, @seed_id, @submission_id, -1, @mu, @sigma);

    -- debug statement
    -- select @seed_id as seed_id, @submission_id as submission_id, @mu as mu, @sigma as sigma;

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

    update matchup
    set map_id = @map_id,
        max_turns = @max_turns
    where matchup_id = @matchup_id;

    -- debug statement
    -- select * from map where map_id = @map_id;

    -- Step 3: select opponents 1 at a time
    set @cur_user_id = @seed_id;
    set @last_user_id = -1;
    set @player_count = 1;

    -- create a list of recent game matchups by user_id
    -- used to keep the matchups even across all users and pairings
    drop temporary table if exists tmp_opponent;
    create temporary table tmp_opponent (
        user_id int,
        opponent_id int,
        game_count int,
        primary key (user_id, opponent_id)
    );

    insert into tmp_opponent
        select user_id, opponent_id, sum(game_count) as game_count
        from (
            select gp1.user_id as user_id,
                gp2.user_id as opponent_id,
                count(*) as game_count
            from game_player gp1
            inner join game g
                on g.game_id = gp1.game_id
            inner join game_player gp2
                on gp2.game_id = g.game_id
            where g.timestamp > timestampadd(hour, -24, current_timestamp)
            and gp1.user_id != gp2.user_id
            group by gp1.user_id, gp2.user_id
            union
            select mp1.user_id as user_id,
                mp2.user_id as opponent_id,
                count(*) as game_count
            from matchup_player mp1
            inner join matchup m
                on m.matchup_id = mp1.matchup_id
            inner join matchup_player mp2
                on mp2.matchup_id = m.matchup_id
            where m.matchup_timestamp > timestampadd(hour, -24, current_timestamp)
            and (m.worker_id >= 0 or m.worker_id is null)
            and m.deleted = 0
            and mp1.user_id != mp2.user_id
            group by mp1.user_id, mp2.user_id
        ) g
        group by 1, 2;

    drop temporary table if exists tmp_games;
    create temporary table tmp_games (
        user_id int,
        game_count int,
        primary key (user_id)
    );
    insert into tmp_games
        select user_id, sum(game_count) as game_count
        from tmp_opponent
        group by user_id;
    
    while @player_count < @players do
                
            -- select list of opponents with match quality
            select s.user_id, s.submission_id, s.mu, s.sigma
            into @last_user_id, @last_submission_id, @last_mu, @last_sigma
            from (
                select @seq := @seq + 1 as seq, s.*
                from (
                    -- list of all submission sorted by match quality
                    select s.user_id, s.submission_id, s.mu, s.sigma
                        -- trueskill match quality for 2 players
                        ,@match_quality := exp(sum(ln(
                            sqrt(@twiceBetaSq / (@twiceBetaSq + pow(p.sigma,2) + pow(s.sigma,2))) *
                            exp(-(pow(p.mu - s.mu, 2) / (2 * (@twiceBetaSq + pow(p.sigma,2) + pow(s.sigma,2)))))
                        ))) as match_quality
                    from
                    submission p, -- current players in match
                    submission s  -- possible next players
                    -- join with all players in current matchup to average match quality
                    where p.submission_id in (
                        select submission_id
                        from matchup_player
                        where matchup_id = @matchup_id
                    )
                    -- exclude players currently in the matchup
                    and not exists (
                        select *
                        from matchup_player mp
                        where mp.matchup_id = @matchup_id
                        and mp.user_id = s.user_id
                    )
                    and s.latest = 1 and s.status = 40
                    group by s.user_id, s.submission_id, s.mu, s.sigma
                    order by 5 desc
                ) s,
                (select @seq := 0) seq
            ) s
            -- join to get total game count for last 24 hours
            left outer join (
                select user_id, count(*) as game_count
                from tmp_games
                group by user_id
            ) r
                on r.user_id = s.user_id
            -- join in user to user game counts to provide round-robin like logic
            left outer join (
                select opponent_id, sum(game_count)
                from tmp_opponent
                where user_id in (
                    select user_id
                    from matchup_player mp
                    where mp.matchup_id = @matchup_id
                )
                group by opponent_id
            ) o
                on o.opponent_id = s.user_id
            -- pareto distribution
            -- the size of the pool of available players will follow a pareto distribution
            -- where the minimum is 5 and 80% of the values will be <= 18
            -- due to the least played ordering, after a submission is established
            -- it will tend to pull from the lowest match quality, so the opponent
            -- rank difference selected will also follow a pareto distribution 
            where s.seq < (5 / pow(rand(), 0.65)) 
            order by o.game_count,
                r.recent_games,
                s.match_quality desc
            limit 1;
                
            -- debug statement
            -- select @last_user_id as user_id, @last_submission_id as submission_id, @last_mu as mu, @last_sigma as sigma;

            -- add new player to matchup
            insert into matchup_player (matchup_id, user_id, submission_id, player_id, mu, sigma)
            values (@matchup_id, @last_user_id, @last_submission_id, -1, @last_mu, @last_sigma);
            set @player_count = @player_count + 1;
            set @cur_user_id = @last_user_id;
            
    end while;

    -- Step 4: put players into map positions
    update matchup_player
    inner join (
        select @position := (@position + 1) as position,
            m.user_id
        from (
            select mp.*
            from matchup_player mp
            where matchup_id = @matchup_id
            order by rand()
        ) m,
        (select @position := -1) p
    ) m2
        on matchup_player.user_id = m2.user_id
    set player_id = m2.position
    where matchup_id = @matchup_id;

    -- debug statement
    -- select * from matchup m inner join matchup_player mp on mp.matchup_id = m.matchup_id where m.matchup_id = @matchup_id;

    -- turn matchup on
    update matchup
    set worker_id = null
    where matchup_id = @matchup_id;

    -- return new matchup id
    select @matchup_id as matchup_id;

end if;

end$$
delimiter ;
