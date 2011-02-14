all: playback/movie.gif


playback/frame_00000.png: *.py
	python playgame.py --timeout_ms 3000 -t 100 -m maps/big.txt "python bots/HunterBot.py" "python bots/MyBot.py" "python bots/LeftyBot.py" "python bots/MyBot.py"

game: playback/frame_00000.png


playback/movie.gif: playback/frame_00000.png
	convert -loop 0 -delay 5 playback/frame*.png -delay 400 `ls playback/frame*.png | sort | tail -n 1` playback/movie.gif
	convert -loop 0 -delay 5 playback/player0*.png -delay 400 `ls playback/player0*.png | sort | tail -n 1` playback/movie_player0.gif
	convert -loop 0 -delay 5 playback/player1*.png -delay 400 `ls playback/player1*.png | sort | tail -n 1` playback/movie_player1.gif
	convert -loop 0 -delay 5 playback/player2*.png -delay 400 `ls playback/player2*.png | sort | tail -n 1` playback/movie_player2.gif
	convert -loop 0 -delay 5 playback/player3*.png -delay 400 `ls playback/player3*.png | sort | tail -n 1` playback/movie_player3.gif
	ln -s playback/movie.gif movie.gif

movie: playback/movie.gif


clean:
	-rm -Rf playback/*.png
	-rm -Rf playback/*.gif
	-rm -Rf movie.gif


.PHONY: all clean movie game
