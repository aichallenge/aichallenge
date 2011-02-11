@echo off
del playback\*.*
playgame.py -t %2 -m %1 "python bots/MyBot.py" "python bots/MyBot.py" "python bots/MyBot.py" "python bots/MyBot.py"
convert -loop 0 -delay 5 playback/frame*start.png playback/movie.gif
convert -loop 0 -delay 5 playback/frame*player0.png playback/movie_player0.gif
convert -loop 0 -delay 5 playback/frame*player1.png playback/movie_player1.gif
convert -loop 0 -delay 5 playback/frame*player2.png playback/movie_player2.gif
convert -loop 0 -delay 5 playback/frame*player3.png playback/movie_player3.gif
