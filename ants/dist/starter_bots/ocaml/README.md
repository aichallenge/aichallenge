# Objective Caml Starter Package for Ant Wars

See the section after "Google AI Challenge Blurb" for information
specific to this OCaml starter package.


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


## OCaml Starter Package Specific Information

## Status

This is a basic implementation of the core functionality and suggested 
helper functions in the Ants Starter Pack guide. Not much testing has 
been done for robustness. 

There is one minor deviation from the starter pack guide: there is an 
optional update_vision function which does a Field of Vision calculation 
for every ant passed in. This allows the visible function to work. You 
don't have to call it, though, and you could implement a per-tile 
visible function which might be more efficient if you're not checking 
many tiles.

## Usage

Build with

   ocamlbuild MyBot.native

in the source directory.

## Where to go from here

Have a look through Ants.mli for provided functions.

MyBot.ml contains the starter bot logic, and Ants.ml implements the I/O 
and helper functions. Users are encouraged to look at Ants.ml, 
particularly at the comments near the end, in the loop function.

Consider modifying the game loop exception handler to suppress fatal 
exceptions and pass the turn rather than complaining and exiting.

Currently the bot doesn't generate any debugging output. There is a 
"ddebug" function which does nothing, but a slight adjustment to some 
comment marks will activate it. I like it to direct the output to a 
file, but you could redirect it to stderr or some other place.

The bot provides issue_order and finish_turn function just as specified 
in the starter pack guide; the functions write directly to stdout. I 
recommend changing this so that issue_order builds up a list of orders, 
checking for errors, and then finish_turn does all the outputting.

This is my first attempt at making something like this for other people 
to use. If anyone would like to make improvements, that would be great. 
Also, testing and bug reports are appreciated.

Note that the starter pack clears all non-water tiles from the map after 
every turn, so if you want it to remember food, you have to either 
change this behaviour in Ants.ml, or implement your own system of 
tracking food. As well as the food marked on the map, there is 
gstate.food which gives a list of food tiles seen this turn.

The loop function does not expect you to modify gstate.tmap, though in 
theory you could. Be careful, have a look at the code to understand the 
consequences. Or work on a copy.

The map is represented by an (int * int) array array.

Those ints represent (tiletype * last_seen_on_turn).

Tiletypes are as follows:

Unseen 	: 0
Land	: 1
Water	: 2
Food	: 3
Ant	: 100+ (100 for friendly, then add owner value for 101, 102...)
Dead ant: 200+ (200 for friendly, then as for Ant)

For the Unseen, Land, Water and Food values, there are functions for 
converting to and from labels of similar names. They are named 
tile_of_int and int_of_tile. They will work for Ant and Dead as well, 
but for those you may want to look at the actual value. int_of_tile 
returns 199 for a live ant, 299 for a dead ant, ignoring owner (if if 
it's yours).

It is still likely that there are undiscovered bugs in this code: 
testing would be great.
