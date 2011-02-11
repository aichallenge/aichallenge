all: playback/movie.gif


playback/frame00001start.png: *.py
	python playgame.py --timeout 3000 -t 500 -m maps/random4.txt "python bots/MyBot.py" "python bots/MyBot.py" "python bots/MyBot.py" "python bots/MyBot.py"

game: playback/frame00001start.png


playback/movie.gif: playback/frame00001start.png
	convert -loop 0 -delay 5 playback/frame*start.png -delay 400 `ls playback/frame*start.png | sort | tail -n 1` playback/movie.gif
	convert -loop 0 -delay 5 playback/frame*player0.png -delay 400 `ls playback/frame*player0.png | sort | tail -n 1` playback/movie_player0.gif
	convert -loop 0 -delay 5 playback/frame*player1.png -delay 400 `ls playback/frame*player1.png | sort | tail -n 1` playback/movie_player1.gif
	convert -loop 0 -delay 5 playback/frame*player2.png -delay 400 `ls playback/frame*player2.png | sort | tail -n 1` playback/movie_player2.gif
	convert -loop 0 -delay 5 playback/frame*player3.png -delay 400 `ls playback/frame*player3.png | sort | tail -n 1` playback/movie_player3.gif
	ln -s playback/movie.gif movie.gif

movie: playback/movie.gif


clean:
	-rm -Rf playback/*.png
	-rm -Rf playback/movie.gif
	-rm -Rf movie.gif


.PHONY: all clean movie game
