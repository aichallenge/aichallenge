#!/usr/bin/env sh
./playgame.py --player_seed 42 --end_wait=0.25 --verbose --log_dir game_logs --turns 100 --map_file maps/symmetric_maps/symmetric_10.map "$@" "python dist/sample_bots/python/HunterBot.py" "python dist/sample_bots/python/LeftyBot.py" "python dist/sample_bots/python/HunterBot.py" "python dist/sample_bots/python/LeftyBot.py"
