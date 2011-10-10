# CoffeeScript Starter Package for Ant Wars

See the section after "Google AI Challenge Blurb" for information
specific to this CoffeeScript starter package.


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


## CoffeeScript Starter Package Specific Information

This rest of this file contains specific information about the CoffeeScript starter package for the [Ant Wars Google AI Challenge](http://ai-contest.com/).

It is assumed you are using
[CoffeeScript](http://jashkenas.github.com/coffee-script/) 1.1.1, since that
is what the challenge server will be using as well.

## Status

This bot is still in development and has only been tested locally with
data on standard input. Nevertheless, patches and improvements are welcome.


## Usage

To test your bot locally you'll need to get the AI
Challenge source tree:

    git clone git://github.com/aichallenge/aichallenge.git

Go to the `aichallenge/ants` directory and do:

    ./playgame.py --end_wait=0.25 --verbose --log_dir game_logs --turns 100 --map_file maps/symmetric_maps/symmetric_10.map "python dist/sample_bots/python/HunterBot.py" "python dist/sample_bots/python/LeftyBot.py" "python dist/sample_bots/python/HunterBot.py" "coffee /path/to/your/MyBot.coffee"

To upload a submission you only need to zip all the files: "`zip
submission.zip *.coffee`".

## Source

`MyBot.coffee` is needed for compilation on the official server. It also contains the "do_turn" function - this is where the starter bot's simple AI resides.

`ants.coffee` contains all other helper functions.

### Internal Map Representation

The map is internally represented as a 2-dimensional array of Objects. It is reset and
modified when "ants.MAP.reset()" is called during the main loop, so work
on a copy if you have to.

Each square of the map can be accessed by ants.MAP[x][y] 
and the values of the squares have to following properties:

MAP[x][y].type is one of {"WATER", "LAND", "DEAD", "FOOD", "ANT", "HILL"}

Additionally squares on the map representing ants have the following properties:

MAP[x][y].is_alive - being set to either "yes" or "no

MAP[x][y].owner - Owner of 0 representing my_ants, and any other owner - enemy_ants.

## Platforms

The code has been tested on the following platforms:

* x86: CoffeeScript 1.1.1., Ubuntu 10.04
