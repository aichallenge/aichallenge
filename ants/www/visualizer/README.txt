1. Online view

To watch a game set up a php enabled web server, and copy any missing files in 'www' over from planet wars.
Set up the mysql database and insert a zlib compressed replay into the 'playback' table as well as some
associated dummy information in the 'game' table.

Then browse to:
http://.../visualizer.html?game_id=1#visualizer


2. Offline viewer

You can also load up 'offline.html' and paste your replay text there.
In that case you only need 'offline.html', 'js/visualizer.js' and no web server.
Please note that a 'img' folder will be added at a later point to the visualizer folder.
