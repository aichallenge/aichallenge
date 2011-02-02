all: playback/movie.gif


playback/frame00001.png: *.py *.png
	python engine.py

game: playback/frame00001.png


playback/movie.gif: playback/frame00001.png
	convert -loop 0 -delay 4 playback/frame*.png -delay 400 playback/`ls playback | sort | tail -n 1` playback/movie.gif
	ln -s playback/movie.gif movie.gif

movie: playback/movie.gif


clean:
	-rm -Rf playback/*.png
	-rm -Rf playback/movie.gif
	-rm -Rf movie.gif


.PHONY: all clean movie game
