#!/usr/bin/env sh
./playgame.py --seed 42 --verbose --output_dir viewer --turns 100 --map_file maps/symmetric_maps/symmetric_10.map "$@" "python dist/sample_bots/python/HunterBot.py" "python dist/sample_bots/python/LeftyBot.py" "python dist/sample_bots/python/HunterBot.py" "python dist/sample_bots/python/LeftyBot.py"

