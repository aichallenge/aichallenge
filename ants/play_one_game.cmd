@echo off
playgame.py --seed 42 --end_wait=2 --verbose --log_dir game_logs --turns 100 --map_file maps/symmetric_maps/symmetric_10.map %* "python dist/sample_bots/python/HunterBot.py" "python dist/sample_bots/python/LeftyBot.py" "python dist/sample_bots/python/HunterBot.py" "python dist/sample_bots/python/LeftyBot.py"

