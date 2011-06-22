@echo off
playgame.py --engine_seed 42 --player_seed 42 --food none --end_wait=0.25 --verbose --log_dir game_logs --turns 30 --map_file submission_test/test.map %1 "python submission_test/TestBot.py" -e --nolaunch --strict --capture_errors
