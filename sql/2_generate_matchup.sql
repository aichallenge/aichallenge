drop procedure if exists generate_matchup;
delimiter $$
create procedure generate_matchup()
begin

select min(players) into @min_players from map;

select count(distinct s.user_id)
into @max_players
from submission s
inner join user u
    on u.user_id = s.user_id
where s.status = 40 and s.latest = 1;

if @min_players <= @max_players then

set @init_mu = 25.0;
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
-- this selects the user that was least recently used player for a seed
-- from both the game and matchup tables
order by m.max_matchup_id asc,
         s.max_game_id asc,
         s.user_id asc
limit 1;

insert into matchup (seed_id, worker_id)
values (@seed_id, 0);
set @matchup_id = last_insert_id();

-- debug statement
-- select 'step 1';

-- Step 2: select the map

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

-- debug statement
-- select 'step 2';

-- Step 2.5: setup matchup and player info for following queries

update matchup
set map_id = @map_id,
    max_turns = @max_turns
where matchup_id = @matchup_id;

insert into matchup_player (matchup_id, user_id, submission_id, player_id, mu, sigma)
values (@matchup_id, @seed_id, @submission_id, -1, @mu, @sigma);

-- debug statement
-- select @matchup_id, @seed_id, @submission_id, @map_id, @players;

-- Step 3: select opponents 1 at a time

-- create temp table to randomize seeding for matching players
drop temporary table if exists temp_random;
create temporary table temp_random 
select u.user_id, rand() as seq
from user u
inner join submission s
    on s.user_id = u.user_id
where s.latest = 1
    and s.status = 40;

-- create temp table to hold user_ids not to match
--   due to repeat games, maps or player_counts
drop temporary table if exists temp_unavailable;
create temporary table temp_unavailable (
 user_id int(11) NOT NULL
);

-- exclude players currently in a matchup
insert into temp_unavailable
select mp.user_id
from matchup_player mp
inner join matchup m
	on mp.matchup_id = m.matchup_id
where (m.worker_id >= 0 or m.worker_id is null)
and m.deleted = 0;

set @last_user_id = -1;
set @cur_user_id = @seed_id;
set @use_limits = 1;
set @abort = 0;
set @player_count = 1;
set @exclude_size = ceil(@max_players * 0.1 / @players);

while @abort = 0 and @player_count < @players do

    -- debug statement
    -- select @matchup_id as matchup_id, @last_user_id as cur_user,
    --        @use_limits as limits, @abort as abort,
    --        @player_count as player_count, @players as total;

    set @last_user_id = @cur_user_id;
    if @use_limits = 1 then

        -- exclude players who played in @last_user_id's last game
        insert into temp_unavailable
        select gp_opponent.user_id
        from submission s
        inner join game_player gp_opponent
            on s.max_game_id = gp_opponent.game_id
        where s.latest = 1
            and s.status = 40
            and s.user_id = @last_user_id
        order by gp_opponent.user_id;

        -- debug statement
		-- select 'step 3: add in users last game';

        -- exclude players who played with @last_user_id last
        insert into temp_unavailable
        select gp_opponent.user_id
        from submission s
        inner join game_player gp_opponent
            on s.user_id = gp_opponent.user_id
            and s.max_game_id = gp_opponent.game_id
        inner join game_player gp_user
            on gp_user.game_id = gp_opponent.game_id
        where s.latest = 1 and s.status = 40
            and gp_user.user_id = @last_user_id
        order by gp_opponent.user_id;

        -- debug statement
		-- select 'step 3: add last with user';
		
    end if;
    
    -- debug statement
    -- select 'step 3: exclude count', count(*) from temp_unavailable;
	
    -- debug statement
	-- select * from temp_unavailable;

    set @last_user_id = -1; -- used to ensure an opponent was selected

    -- pick the closest 100 available submissions (limited for speed)
    -- and then rank by a match_quality approximation
    --   the approximation is not the true trueskill match_quality,
    --   but will be in the same order as if we calculated it
    select s.user_id, s.submission_id, s.mu, s.sigma
    ,@c := (@twiceBetaSq + pow(mp.sigma,2) + pow(s.sigma,2)) as c,
    exp(sum(ln(
        SQRT(@twiceBetaSq / @c) * EXP(-(pow(mp.mu - s.mu, 2) / (2 * @c)))
    ))) as match_quality
    into @last_user_id, @last_submission_id, @last_mu, @last_sigma , @c, @match_quality
    from matchup_player mp,
    (
        select s.user_id, s.submission_id, s.mu, s.sigma, s.game_count, t.seq
        from submission s
        inner join temp_random t
            on s.user_id = t.user_id
        ,(select avg(mu) as avg_mu
            from matchup_player
            where matchup_id = @matchup_id) avg_mu
        where s.latest = 1 and status = 40
        and s.user_id not in (
            select user_id from temp_unavailable
        )

        -- this order by causes a filesort, but I don't see a way around it
        -- limiting to 100 saves us from doing extra trueskill calculations
        order by abs(s.mu - avg_mu.avg_mu) asc,
                 s.game_count asc
                 ,t.seq asc
        limit 100
    ) s
    -- left outer join temp_unavailable tu
    --     on s.user_id = tu.user_id
    where mp.matchup_id = @matchup_id
    -- and tu.user_id is null
    group by s.user_id, s.submission_id, s.mu, s.sigma
    order by match_quality desc,
             s.game_count asc
             ,s.seq asc
    limit 1;

    if @last_user_id = -1 then
    
		-- debug statement
		-- select 'step 3: dropping limits';
		
        if @use_limits = 0 then
            set @abort = 1;
        end if;
        -- excluded too many players
        -- drop limits and reset to players in matchup
        delete from temp_unavailable;
        insert into temp_unavailable
        select user_id
        from matchup_player
        where matchup_id = @matchup_id;
        set @use_limits = 0;
    else
    
		-- debug statement
		-- select @last_user_id, user_id from matchup_player where matchup_id = @matchup_id;
		
		-- add new player to matchup and exclude list
        insert into matchup_player (matchup_id, user_id, submission_id, player_id, mu, sigma)
        values (@matchup_id, @last_user_id, @last_submission_id, -1, @last_mu, @last_sigma);
        insert into temp_unavailable (user_id) values (@last_user_id);
        set @player_count = @player_count + 1;
        set @cur_user_id = @last_user_id;
        
        -- debug statement
        -- select * from temp_unavailable;
        -- select 'added user';
        
    end if;

end while;

if @abort = 1 then

    delete from matchup_player where matchup_id = @matchup_id;
    delete from matchup where matchup_id = @matchup_id;

else

-- debug statement
-- select 'step 3';

-- Step 4: put players into map positions

update matchup_player
inner join (
    select @position := (@position + 1) as position,
        m.user_id
    from (
        select mp.*
        from matchup_player mp
        inner join temp_random r
            on r.user_id = mp.user_id
        where matchup_id = @matchup_id
        order by r.seq
    ) m,
    (select @position := -1) p
) m2
    on matchup_player.user_id = m2.user_id
set player_id = m2.position
where matchup_id = @matchup_id;

-- since we are using symmetric maps, positions don't matter
-- the following loop will not execute
-- set @player_count = 0;
set @player_count = @players;

while @player_count < @players do

    select mp.user_id, id.player_id
    into @pos_user_id, @pos_player_id
    from matchup_player mp
    inner join (
        select user_id
        from matchup_player mp
        where matchup_id = @matchup_id
        and player_id = -1
    ) avail
        on mp.user_id = avail.user_id,
    (
        select @row := @row + 1 as player_id
        from matchup_player,
        (select @row := -1) id
        where matchup_id = @matchup_id
    ) id
    where mp.matchup_id = @matchup_id
    and id.player_id not in (
        select player_id
        from matchup_player mp
        where matchup_id = @matchup_id
        and player_id != -1
    )
    order by (mp.user_id = @seed_id) desc,
    (
        select max(g.game_id)
        from game g
        inner join game_player gp
            on g.game_id = gp.game_id
        where g.map_id = @map_id
        and gp.user_id = mp.user_id
        and gp.player_id = id.player_id
    ) asc
    limit 1;

    update matchup_player
    set player_id = @pos_player_id
    where matchup_id = @matchup_id
    and user_id = @pos_user_id;

    set @player_count = @player_count + 1;

end while;

-- debug statement
-- select 'step 4';

-- turn matchup on
update matchup
set worker_id = null
where matchup_id = @matchup_id;

-- select @max_players, @min_players;

-- return new matchup id
select @matchup_id as matchup_id;

end if;

end if;

end$$
delimiter ;
