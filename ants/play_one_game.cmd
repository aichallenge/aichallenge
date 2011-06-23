@echo off
python %~dp0playgame.py --engine_seed 42 --player_seed 42 --end_wait=0.25 --verbose --log_dir game_logs --turns 1000 --map_file %~dp0maps\symmetric_maps\symmetric_10.map %* "python %~dp0dist\sample_bots\python\HunterBot.py" "python %~dp0dist\sample_bots\python\LeftyBot.py" "python %~dp0dist\sample_bots\python\HunterBot.py" "python %~dp0dist\sample_bots\python\LeftyBot.py"

