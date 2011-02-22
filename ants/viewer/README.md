Visualizer for Testing
======================

Run the following command in the visualizer direction to ensure the jquery $.get
call works as expected:

    python -m SimpleHTTPServer
    
This will serve up the visualizer directory as localhost:8000.  Then browse to
http://localhost:8000/viewer.html to view the latest game.  The make_movie.cmd
file is set to put the replay in the visualizer directory.

