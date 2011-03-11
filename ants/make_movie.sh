#!/usr/bin/sh
map="${@:(-2):1}"
turns="${@:(-1):1}"
python playgame.py -r 1 -o viewer -t "$turns" -m "$map" "${@:1:$(($#-2))}" "python bots/python/HunterBot.py" "python bots/python/LeftyBot.py" "python bots/python/HunterBot.py" "python bots/python/LeftyBot.py"

