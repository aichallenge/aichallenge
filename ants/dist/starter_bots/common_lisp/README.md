# Common Lisp Starter Package for Ant Wars

See the section after "Google AI Challenge Blurb" for information
specific to this Common Lisp (CL) starter package.


## Google AI Challenge Blurb

The files in this package are part of a starter package from the
Google AI Challenge. The Google AI Challenge is an Artificial
Intelligence programming contest. You can get more information by
visiting [www.ai-contest.com](http://www.ai-contest.com/).

The entire contents of this starter package are released under the
Apache license as is all code related to the Google AI Challenge. See
[http://github.com/aichallenge/aichallenge](http://github.com/aichallenge/aichallenge) for more details.

There are a bunch of tutorials on the [ai-contest.com](http://ai-contest.com/)
website that tell you what to do with the contents of this starter
package. For the impatient, here is a brief summary.

* In the root directory, there are a bunch of code files. These are a
  simple working contest entry that employs a basic strategy. These
  are meant to be used as a starting point for you to start writing
  your own entry.  Alternatively, you can just package up the starter
  package as-is and submit it on the website.

* The tools directory contains a game engine and visualizer. This is
  meant to be used to test your bot. See the relevant tutorials on the
  website for information about how to use the tools.

* The example_bots directory contains some sample bots for you to test
  your own bot against.


## Common Lisp Starter Package Specific Information

This is the basic CL starter package, a more extensive starter package
with [proxy-bot](http://ai-contest.com/forum/viewtopic.php?f=19&t=468)
functionality can be found at [http://github.com/aerique/google-ai-challenge-2011-1-ants/tree/master/common-lisp-starter-package](http://github.com/aerique/google-ai-challenge-2011-1-ants/tree/master/common-lisp-starter-package).

This rest of this file contains specific information about the Common
Lisp starter package for the [Ant Wars Google AI Challenge](http://ai-contest.com/).

It is assumed you are using [SBCL](http://www.sbcl.org/), since that
is what the challenge server will be using as well.  Feel free to try
any CL implementation locally but do realize SBCL is used on the
official tournament server.  Currently the official server is running
[Ubuntu 10.10/"Maverick Meerkat" and thus SBCL 1.0.40](http://packages.ubuntu.com/maverick/devel/sbcl).


## Status

This bot is still in development and has only been tested locally with
data on standard input and with my own game engine (see the extensive
starter package).  Nevertheless, patches and improvements are welcome.


## Usage

To compile the source files into a `MyBot` binary issue "`sbcl --script
MyBot.lisp`".  To test your bot locally you'll need to get the AI
Challenge source tree:

    git clone git://github.com/aichallenge/aichallenge.git

Go to the `aichallenge/ants` directory and do:

    ./playgame.py --end_wait=0.25 --verbose --log_dir game_logs --turns 100 --map_file maps/symmetric_maps/symmetric_10.map "python dist/sample_bots/python/HunterBot.py" "python dist/sample_bots/python/LeftyBot.py" "python dist/sample_bots/python/HunterBot.py" /path/to/your/MyBot

To upload a submission you only need to zip all the lisp-files: "`zip
submission.zip *.lisp`".  ("`make submission-zip`" will do the same).

### Windows / MSYS Note

You're probably best of putting a symbolic link `sbcl` in /usr/bin
pointing to wherever SBCL is installed on your system.


## Source

`MyBot.lisp` is needed for compilation on the official server.

`main.lisp` contains the main loop and the DO-TURN function, this is
were the starter bot's simple AI resides.

`ants.lisp` contains all other helper functions.

### Internal Map Representation

The map is internally represented as a 2-dimensional array of [fixnums](file:///export/home/ekwis/emacs/HyperSpec/Body/t_fixnum.htm#fixnum)
and is accessible as `(game-map *state*)`.  Do note that it is reset and
modified when PARSE-GAME-STATE is called during the main loop, so work
on a copy if you have to.

Each tile of the map can be accessed by `(aref (game-map *state) row col)`
and the values of the tiles have to following meaning:

0 = land  
1 = water  
2 = food  
100+ = live ant  
200+ = dead ant  

Your live ants are always represented as 100 and your dead ants as
200.


## Platforms

The code has been tested on the following platforms:

* x86: SBCL 1.0.45.debian

### Windows Note

If you're running Windows it is assumed you are running [MSYS](http://www.mingw.org/node/18).
