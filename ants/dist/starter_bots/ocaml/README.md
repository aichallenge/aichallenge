# Objective Caml Starter Package for Ant Wars

NOTE - Anthills were added after most of this material was written, and it
has not been updated.

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

This is an implementation of the core functionality and suggested helper 
functions in the Ants Starter Pack guide.

There is an optional update_vision function which does a Field of Vision 
calculation for every ant passed in. This allows the visible function to 
work. You don't have to call it, though, and you could implement a 
per-tile visible function which might be more efficient if you're not 
checking many tiles. 

## Usage

Build with

   ocamlbuild MyBot.native

in the source directory.

## Where to go from here

Here are some incremental improvements:

 * prevent ant collisions

   Updating the state of the game map is one obvious way to do this.
This will also allow other ants to see that it's okay to move into the
space which is going to be vacated.

   Many other possibilities exist, such as maintaining a list of
destinations, and they may have benefits.

 * move toward food and enemies

   Even without pathfinding, a bot which tries to walk straight toward
food or enemies will usually outperform the starter.

 * explore the unknown

   A bot which sits still when it can't see anything interesting is
missing an opportunity to expand. Even if you just make it try random
steps for every ant who doesn't know what to do, this will generally
outperform the do-nothing approach.

 * search for the shortest path

   Implementing a breadth first search and searching outward from all
food tiles simultaneously can tell every ant which direction to step in
to find the nearest food. No more getting stuck against water.

 * efficient food collection

   Sending ten ants to collect one piece of food isn't very efficient.
Finding ways to minimize the number of ants devoted to food collection
allows more ants to be used for exploration and combat.

 * consider battles

   Battles are complex, and the approaches will be varied, but adding
some sort of combat awareness makes a bot far more effective.

 * broader strategy

   After playing around with some of the above and watching some games, 
it's worth thinking about what seems effective, what you can process in 
one second, and what you can implement in the time available.

## Some notes I wrote while getting this ready:

Please note that most of these comments were written before I added the 
OO interface. I've updated some things but may have missed some.

The main change resulting from this is that you will need to add some 
getters to the swrap class in Ants.ml when you want to access bits of 
game state. After doing that, you will need to either hide (rename) or 
recreate the Ants.mli file with "ocamlc -i", because it won't know 
about your new getters. See methods my_ants and get_map if you don't 
understand what I mean by "getters".

You don't have to use the OO interface. You can change the loop function 
in Ants.ml to provide the raw tgame_state, recreate (or remove) 
Ants.mli, and use the raw functions which are called by the swrap class. 
You could also revert the ant object to a record with associated 
functions.

The advantage of the OO interface is that some function calls require 
fewer arguments, because they can be referenced internally.

Have a look through the default Ants.mli for provided functions.

MyBot.ml contains the starter bot logic, and Ants.ml implements the I/O 
and helper functions. Users are encouraged to look at Ants.ml, 
particularly at the comments near the end, relating to the loop 
function.

The bot directs its debugging info to stderr. Under normal conditions, 
the starter pack should say nothing, but if things go wrong it will 
generate some output. If you change the ddebug function in Ants.ml to a 
unit () then the program will not generate any debugging output at all.

The pack provides issue_order and finish_turn functions just as 
specified in the starter pack guide; the functions write directly to 
stdout. 

This is my first attempt at making something like this for other people 
to use. If anyone would like to make improvements, that would be great. 
Also, testing and bug reports are appreciated.

Food you have seen this turn is marked on the map, and cleared between 
turns. As well as the food on the map, the game state object contains 
all the food tiles seen this turn as a (row, col) list.

Ants.ml does not expect you to modify gstate.tmap, though in theory you 
could. Be careful, have a look at the code to understand the 
consequences. Or work on a copy.

The map is represented by a mapb array array, with mapb consisting of:

type mapb {seen : int; content : int}

seen is the turn on which the tile was last seen, so the visible 
function can check if the tile was seen this turn.

Tiletypes (content) are as follows:

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
returns 199 for a live ant, 299 for a dead ant, ignoring owner (even if 
it's yours) so for those cases, check the ints.

It is still likely that there are undiscovered bugs in this code: 
testing would be great.
