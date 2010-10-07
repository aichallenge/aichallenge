#!/bin/bash
cd /home/contest/ai-contest/planet_wars/backend/;
for (( ; ; )); do   sudo -u contest python tournament_manager.py 575 > /dev/null 2> /dev/null; sleep 5; done