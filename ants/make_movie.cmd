@echo off
playgame.py -r 1 -o output -t %2 -m %1 "java -jar bots/java/LeftyBot.jar" "java -jar bots/java/HunterBot.jar" "java -jar bots/java/LeftyBot.jar" "java -jar bots/java/HunterBot.jar"

