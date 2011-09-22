drop procedure if exists match_quality;
delimiter $$

create procedure match_quality(in the_user_id int)
begin

select s.user_id, s.submission_id, s.mu, s.sigma
into @seed_id, @submission_id, @mu, @sigma
from submission s
where s.user_id = the_user_id
    and s.latest = 1 and s.status = 40;
      
-- list of all submission sorted by match quality
-- setup trueskill calc values
set @init_mu = 50.0;
set @init_beta = @init_mu / 6;
set @twiceBetaSq = 2 * pow(@init_beta, 2);

select s.user_id, s.submission_id, s.mu, s.sigma, s.rank
    -- trueskill match quality for 2 players
    ,@match_quality := sqrt(@twiceBetaSq / (@twiceBetaSq + pow(p.sigma,2) + pow(s.sigma,2))) *
        exp(-(pow(p.mu - s.mu, 2) / (2 * (@twiceBetaSq + pow(p.sigma,2) + pow(s.sigma,2)))))
    as match_quality
from
submission p, -- current players in match
submission s  -- possible next players
-- join with all players in current matchup to average match quality
where p.submission_id = @submission_id
and s.latest = 1 and s.status = 40
group by s.user_id, s.submission_id, s.mu, s.sigma, s.rank
order by 6 desc
limit 30;

select s.user_id, s.submission_id, s.mu, s.sigma, s.rank
    -- trueskill match quality for 2 players
    ,@match_quality := exp(sum(ln(sqrt(@twiceBetaSq / (@twiceBetaSq + pow(p.sigma,2) + pow(s.sigma,2))) *
        exp(-(pow(p.mu - s.mu, 2) / (2 * (@twiceBetaSq + pow(p.sigma,2) + pow(s.sigma,2)))))
    ))) as match_quality
from
submission p, -- current players in match
submission s  -- possible next players
-- join with all players in current matchup to average match quality
where p.submission_id = @submission_id
and s.latest = 1 and s.status = 40
group by s.user_id, s.submission_id, s.mu, s.sigma, s.rank
order by 6 desc
limit 30;

select @seed_id as seed_id, @submission_id as submission_id, @mu as mu, @sigma as sigma, rank
from submission
where submission_id = @submission_id;

end$$
delimiter ;
