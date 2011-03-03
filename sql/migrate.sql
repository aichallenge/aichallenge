-- migrate user_status_code
delete from aichallenge.user_status_code;
alter table aichallenge.user_status_code auto_increment = 0;
insert into aichallenge.user_status_code (status_id, name)
select status_id, name from planetwars.user_status_codes;

-- migrate country
delete from aichallenge.country;
alter table aichallenge.country auto_increment = 0;
insert into aichallenge.country (country_id, country_code, name, flag_filename)
select country_id, country_code, name, flag_filename from planetwars.countries;

-- migrate organization
delete from aichallenge.organization;
alter table aichallenge.organization auto_increment = 0;
insert into aichallenge.organization (org_id, name)
select org_id, name from planetwars.organizations;

-- migrate language
delete from aichallenge.language;
alter table aichallenge.language auto_increment = 0;
insert into aichallenge.language (language_id, name, main_code_file, command, platform_specific_compilation)
select language_id, name, main_code_file, command, platform_specific_compilation from planetwars.language;

-- migrate user
delete from aichallenge.user;
alter table aichallenge.user auto_increment = 0;
insert into aichallenge.user (user_id, username, password, email, status_id, activation_code, org_id, bio, country_id, created, activated, admin)
select user_id, username, password, email, status_id, activation_code, org_id, bio, country_id, created, activated, admin  from planetwars.users;
