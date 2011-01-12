all: playback/movie.gif

playback/frame00001.png:
	python engine.py

playback/movie.gif: playback/frame00001.png
	convert -loop 0 playback/frame*.png playback/movie.gif
	ln -s playback/movie.gif movie.gif

clean:
	-rm -Rf playback/*.png
	-rm -Rf playback/movie.gif
	-rm -Rf movie.gif
