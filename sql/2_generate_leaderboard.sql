drop procedure if exists generate_leaderboard;
delimiter $$
create procedure generate_leaderboard()
begin

set @last_leader = (select max(leaderboard_id) from leaderboard);

insert into leaderboard (timestamp, algorithm_name, calculation_time, complete)
values (now(), 'TrueSkill', 0, 1);

set @leader = last_insert_id();

insert into ranking (leaderboard_id, user_id, submission_id, version, seq, rank, rank_change, skill, skill_change, latest, age)
select
    @leader as leaderboard_id,
    s.user_id, s.submission_id, s.version,
    @count1 := @count1 + 1 as seq,
    @rank := if(s.latest > 0, @count2 := @count2 + 1 , null) as rank,
    r.rank - @rank as rank_change, s.mu as skill, s.mu - r.skill as skill_change, s.latest, timediff(now(),s.timestamp) as age
from
    submission s
    -- inner join to ensure both user and submission record exists
    inner join user u
        on s.user_id = u.user_id
    left outer join ranking r
        on s.submission_id = r.submission_id and r.leaderboard_id = @last_leader,
    (select @count1 := 0) c1,
    (select @count2 := 0) c2
where s.latest = 1
or s.submission_id in (
    select submission_id
    from ranking
    where leaderboard_id = @last_leader
    and seq in (
        select min(seq)
        from ranking
        where leaderboard_id = @last_leader
        group by user_id
    )
)
order by s.mu desc, s.sigma asc, s.timestamp asc;

update submission
set sigma = sigma + (0.1 * (
    select count(*) from (select * from submission) s where timestamp > (
        select timestamp from leaderboard where leaderboard_id = @last_leader
    )
))
where latest = 1;

end$$
delimiter ;
