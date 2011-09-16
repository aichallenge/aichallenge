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
select mp.user_id, mp.submission_id, mp.mu, mp.sigma, s.user_id, s.submission_id, s.mu, s.sigma
,@c := (@twiceBetaSq + pow(mp.sigma,2) + pow(s.sigma,2)) as c,
round(SQRT(@twiceBetaSq / @c) * EXP(-(pow(mp.mu - s.mu, 2) / (2 * @c))),2) as match_quality,
round(mp.mu - s.mu,2) as mu_diff, round(mp.sigma - s.sigma,2) as sigma_diff
from submission mp,
submission s
where mp.submission_id = @submission_id
and s.latest = 1 and s.status = 40
order by match_quality desc,
         s.game_count asc
limit 100;

end$$
delimiter ;
