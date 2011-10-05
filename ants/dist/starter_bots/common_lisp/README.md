# Common Lisp Starter Package for Ant Wars

This is the basic CL starter package, a more extensive starter package
with [proxy-bot](http://ai-contest.com/forum/viewtopic.php?f=19&t=468)
functionality can be found at [http://github.com/aerique/google-ai-challenge-2011-1-ants/tree/master/common-lisp-starter-package](http://github.com/aerique/google-ai-challenge-2011-1-ants/tree/master/common-lisp-starter-package).

This rest of this file contains specific information about the Common
Lisp starter package for the [Ant Wars Google AI Challenge](http://ai-contest.com/).

It is assumed you are using [SBCL](http://www.sbcl.org/), since that
is what the challenge server will be using as well.  Feel free to try
any CL implementation locally but do realize SBCL is used on the
official tournament server.


## Usage

To compile the source files into a `MyBot` binary issue "`sbcl --script
MyBot.lisp`".  To test your bot locally you'll need to get the AI
Challenge source tree:

    git clone git://github.com/aichallenge/aichallenge.git

Go to the `aichallenge/ants` directory and do:

    ./playgame.py --end_wait=0.25 --verbose --log_dir game_logs --turns 100 --map_file maps/symmetric_maps/symmetric_10.map "python dist/sample_bots/python/HunterBot.py" "python dist/sample_bots/python/LeftyBot.py" "python dist/sample_bots/python/HunterBot.py" /path/to/your/MyBot

To upload a submission you only need to zip all the lisp-files: "`zip
submission.zip *.lisp`".  ("`make submission-zip`" will do the same).

### Submission Errors

If SBCL does any output on standard error (stderr / \*error-output*)
it will count as a compilation error to the server.  So even innocuous
compiler notes or warnings will cause a compilation error.

I've added two statements to MyBot.lisp that should muffle most of the
warnings, but I'm not sure they will catch them all.  If really
necessary redirect \*error-output* to \*standard-output* like so:
`(setf *error-output* *standard-output*)` at the top of MyBot.lisp.

**However**, this will also hide genuine compilation errors that would
otherwise be shown on your profile page!  So if your bot still fails
compiling on the server, your best best is resubmitting with the
redirection disabled.

### Windows / MSYS Note

You're probably best of putting a symbolic link `sbcl` in /usr/bin
pointing to wherever SBCL is installed on your system.

It is assumed you are running [MSYS](http://www.mingw.org/node/18).


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
