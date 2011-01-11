#!/bin/sh

cd /home/contest/ai-contest/planet_wars/backend/
# clear out any jail users still marked in use from a previous crash
echo 'UPDATE jail_users set in_use=0' | mysql contest
# start tournament's running
/home/contest/ai-contest/planet_wars/backend/tournament_manager_runner.sh &
/home/contest/ai-contest/planet_wars/backend/tournament_manager_runner.sh &
