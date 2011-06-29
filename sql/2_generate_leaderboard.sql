drop procedure if exists generate_leaderboard;
delimiter $$
create procedure generate_leaderboard()
begin

set @last_leader = (select max(leaderboard_id) from leaderboard);

insert into leaderboard (timestamp, algorithm_name, calculation_time, complete, last_game_id)
values (now(), 'TrueSkill', 0, 1, (select max(game_id) from game));

set @leader = last_insert_id();

insert into ranking (leaderboard_id, user_id, submission_id, version, seq,
                     rank, rank_change, mu, mu_change, sigma, sigma_change,
                     skill, skill_change, latest, age)
select
    @leader as leaderboard_id,
    s.user_id, s.submission_id, s.version,
    @count1 := @count1 + 1 as seq,
    @rank := if(s.latest > 0, @count2 := @count2 + 1 , null) as rank,
    gp.rank - @rank as rank_change,
    s.mu as mu, gp.mu_after - gp.mu_before as mu_change,    
    s.sigma as sigma, gp.sigma_after - gp.sigma_before as sigma_change,
    @skill := if(3*s.sigma>s.mu, 0.0, s.mu - s.sigma * 3) as skill,
    (gp.mu_after - gp.sigma_after * 3) - (gp.mu_before - gp.sigma_before * 3) as skill_change,
    s.latest, timediff(now(),s.timestamp) as age
from
    (select * from submission order by mu - sigma * 3 desc, mu desc, sigma asc, submission_id asc) s
    -- inner join to ensure both user and submission record exists
    inner join user u
        on s.user_id = u.user_id
    -- these 3 joins bring in the last game_player and game for a submission
    left outer join (select submission_id, max(game_id) as game_id
                     from game_player
                     group by submission_id) gpmax
        on gpmax.submission_id = s.submission_id
    left outer join game_player gp
        on gp.submission_id = gpmax.submission_id
        and gp.game_id = gpmax.game_id
    left outer join game g
        on g.game_id = gpmax.game_ID,
    (select @count1 := 0) c1,
    (select @count2 := 0) c2
where s.latest = 1
-- ghost requirements
or (
    -- can only be 48 hours old
    g.timestamp > timestampadd(hour, -48, current_timestamp)
    -- must have smaller sigma (more confident than current)
    and s.sigma = (select min(sigma)
                   from submission s2
                   where s2.user_id = s.user_id)
    -- must be a higher rank
    and (s.mu - s.sigma * 3) > (select mu - sigma * 3
                                from submission s3
                                where s3.user_id = s.user_id
                                and latest = 1)
    );

-- sigma will be updated on a per game basis
-- update submission
-- set sigma = sigma + (0.1 * (
--     select count(*) from (select * from submission) s where timestamp > (
--         select timestamp from leaderboard where leaderboard_id = @last_leader
--     )
-- ))
-- where latest = 1;

end$$
delimiter ;
