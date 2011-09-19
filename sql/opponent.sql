drop procedure if exists opponent;
delimiter $$
create procedure opponent()
begin

set @init_mu = 25.0;
set @init_sigma = @init_mu/3;
set @beta = @init_sigma/2;
set @tau = @init_sigma/100;
set @2betasq = 2 * pow(@beta, 2);

-- select seed player
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

set @init_mu = 50.0;
set @init_beta = @init_mu / 6;
set @twiceBetaSq = 2 * pow(@init_beta, 2);

-- select list of opponents with match quality
select * from (

select s.*, @seq := @seq + 1 as seq, @count as count
from (

select mp.user_id as p_user_id, mp.submission_id as p_submission_id, mp.mu as p_mu, mp.sigma as p_sigma,
    s.user_id, s.submission_id, s.mu, s.sigma, s.game_count
,@c := (@twiceBetaSq + pow(mp.sigma,2) + pow(s.sigma,2)) as c,
round(SQRT(@twiceBetaSq / @c) * EXP(-(pow(mp.mu - s.mu, 2) / (2 * @c))),4) as match_quality,
round(mp.mu - s.mu,2) as mu_diff, round(mp.sigma - s.sigma,3) as sigma_diff,
recent.game_count as recent_count
from submission mp,
submission s
left outer join (
    select gp.user_id, count(*) as game_count
    from game_player gp
    inner join game g
        on g.game_id = gp.game_id
    where g.timestamp > timestampadd(day, -1, current_timestamp)
    group by gp.user_id
) as recent
    on s.user_id = recent.user_id
where mp.submission_id = @submission_id
and s.latest = 1 and s.status = 40
order by match_quality desc,
         s.game_count asc

) s,
(select @seq := 0) seq,
(select @count := (select count(*) from submission where latest = 1 and status = 40)) count
where @seq < @count / 10
) t
order by recent_count, match_quality
;

end$$
delimiter ;
