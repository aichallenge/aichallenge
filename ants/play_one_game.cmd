@echo off
playgame.py --seed 42 --verbose --output_dir viewer --turns 100 --map_file maps/random4.txt "python dist/sample_bots/python/HunterBot.py" "python dist/sample_bots/python/LeftyBot.py" "python dist/sample_bots/python/HunterBot.py" "python dist/sample_bots/python/LeftyBot.py"

