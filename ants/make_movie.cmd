@echo off
del playback\*.* /q
playgame.py --timeout_ms 30000 -t %2 -m %1 "python bots/MyBot.py" "python bots/HunterBot.py" "python bots/MyBot.py" "python bots/LeftyBot.py"
echo Creating movies
convert -loop 0 -delay 5 playback/frame*.png playback/movie.gif
convert -loop 0 -delay 5 playback/player0*.png playback/movie_player0.gif
convert -loop 0 -delay 5 playback/player1*.png playback/movie_player1.gif
convert -loop 0 -delay 5 playback/player2*.png playback/movie_player2.gif
convert -loop 0 -delay 5 playback/player3*.png playback/movie_player3.gif
