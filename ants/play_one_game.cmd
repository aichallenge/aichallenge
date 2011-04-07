@echo off
playgame.py --seed 42 --verbose --output_dir viewer --turns 100 --map_file maps/symmetric/symmetric_10.map "C:\python31\python.exe dist/sample_bots/python/HunterBot.py" "C:\python31\python.exe dist/sample_bots/python/LeftyBot.py" "C:\python31\python.exe dist/sample_bots/python/HunterBot.py" "C:\python31\python.exe dist/sample_bots/python/LeftyBot.py"

