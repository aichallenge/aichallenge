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
order by 5 desc;

select @seed_id as seed_id, @submission_id as submission_id, @mu as mu, @sigma as sigma, rank
from submission
where submission_id = @submission_id;

end$$
delimiter ;
