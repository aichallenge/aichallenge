@echo off
del playback\*.* /q
playgame.py --timeout_ms 1000 -t %2 -m %1 "java -jar bots/java/LeftyBot.jar" "java -jar bots/java/HunterBot.jar" "java -jar bots/java/HunterBot.jar" "java -jar bots/java/HunterBot.jar"
echo Creating movies
convert -loop 0 -delay 5 playback/frame*.png playback/movie.gif
rem convert -loop 0 -delay 5 playback/player0*.png playback/movie_player0.gif
rem convert -loop 0 -delay 5 playback/player1*.png playback/movie_player1.gif
rem convert -loop 0 -delay 5 playback/player2*.png playback/movie_player2.gif
rem convert -loop 0 -delay 5 playback/player3*.png playback/movie_player3.gif
