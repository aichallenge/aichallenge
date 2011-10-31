-- create table of possible dup users
drop table if exists dup_user;
create table dup_user
select naive_ip, count(distinct login_attempt.username) user_count
from login_attempt
inner join user
    on user.username = login_attempt.username
group by naive_ip
having count(distinct login_attempt.username) > 1;
 
-- get info about dup users
select naive_ip, real_ip, login_attempt.username, user.email, count(*)
from login_attempt
inner join user
    on user.username = login_attempt.username
where naive_ip in (
    select naive_ip
    from dup_user
)
group by naive_ip, real_ip, login_attempt.username, email
order by naive_ip, real_ip, login_attempt.username, email;
