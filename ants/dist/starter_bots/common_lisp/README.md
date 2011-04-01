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

Run `bin/run-ants-bot.sbcl` and paste the sample input from the [Bot Input](http://github.com/aichallenge/aichallenge/wiki/Ant-bot-input-output)
wiki page.

You can also run either `make` or `make local-bot` to compile the bot
into a `MyBot` binary.  The former is done on the official server and
the latter (`make local-bot`) will create a binary that logs to a
`MyBot.log` file.

There are also a few initial unit tests which can be run by issuing
`bin/run-tests.sh`.

### Windows / MSYS Note

You're probably best of putting a symbolic link `sbcl` in /usr/bin
pointing to wherever SBCL is installed on your system.


## Source

`src/ants-bot.lisp` contains the game loop and `bot-think.lisp`
contains the actual bot AI.  Currently it does nothing more than check
whether it can go either north, east, south or west and if it can it
issues an order to the server to go in that direction.  (As described
in the [Ants Starter Pack Guide](https://github.com/aichallenge/aichallenge/wiki/Ants-Starter-Pack-Guide)).

See the `state` class in `src/config.lisp` for the game variables.

### Internal Map Representation

Also see `src/game-state.lisp`.

The map is internally represented as a 2-dimensional array of [fixnums](file:///export/home/ekwis/emacs/HyperSpec/Body/t_fixnum.htm#fixnum)
and is accessible as `(game-map *state*)`.  Do note that is reset and
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


### 3rd Party Libraries

There's already infrastructure to use 3rd party libraries, see the
extensive starter bot mentioned earlier for examples of using them.


## Platforms

The code has been tested on the following platforms:

* x86: SBCL 1.0.45.debian
* x86: [Experimental SBCL 1.0.45 with threads](https://sites.google.com/site/dmitryvksite/sbcl-distr) for Windows using [MSYS](http://www.mingw.org/node/18)
* x86: SBCL 1.0.40 on an [Ubuntu 10.10 VirtualBox image](http://virtualboxes.org/images/ubuntu/)

### Windows Note

If you're running Windows it is assumed you are running [MSYS](http://www.mingw.org/node/18).
