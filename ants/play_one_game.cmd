@echo off
python %~dp0playgame.py --seed 42 --end_wait=0.25 --verbose --log_dir game_logs --turns 100 --map_file %~dp0maps\symmetric_maps\symmetric_10.map %* "python %~dp0sample_bots\python\HunterBot.py" "python %~dp0sample_bots\python\LeftyBot.py" "python %~dp0sample_bots\python\HunterBot.py" "python %~dp0sample_bots\python\LeftyBot.py"

