# Scala Starter Package for Ant Wars

See the section after "Google AI Challenge Blurb" for information
specific to this Scala starter package.


## Google AI Challenge Blurb

The files in this package are part of a starter package from the
Google AI Challenge. The Google AI Challenge is an Artificial
Intelligence programming contest. You can get more information by
visiting [www.ai-contest.com](http://www.ai-contest.com/).

The entire contents of this starter package are released under the
Apache license as is all code related to the Google AI Challenge. See
[code.google.com/p/ai-contest/](http://code.google.com/p/ai-contest/)
for more details.

There are a bunch of tutorials on the
[ai-contest.com](http://ai-contest.com/) website that tell you what to
do with the contents of this starter package. For the impatient, here
is a brief summary.

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


## Scala Starter Package Specific Information

This is the basic Scala starter package. It is implemented as per the
requirements for a starter package as documented on the 
[Google AI Challenge wiki](https://github.com/aichallenge/aichallenge/wiki/Ants-Starter-Pack-Guide).

The official server will be executing Scala bots using [Scala 2.8.1-final](http://www.scala-lang.org/node/8102).
Contestants are recommended to use this version for their development.


## Source

`AntsGame.scala` contains the game loop. `Parser.scala` contains the input 
parsing logic. `MyBot.scala` contains the actual bot AI. Currently it does 
nothing more than check whether it can go either north, east, south or west 
and if it can it issues an order to the server to go in that direction.  
(As described in the [Ants Starter Pack Guide](https://github.com/aichallenge/aichallenge/wiki/Ants-Starter-Pack-Guide)).

The `Bot.scala` trait, from which `MyBot.scala` derives, provides the 
interface for your bot.

    def ordersFrom(gameState: Game): Set[Order]

Given a `GameState` (being all that your bot has been told about the game 
state in the current round; all bodies of water ever seen; and the
parameters used to seed the game), your bot is tasked with returning a
set of `Order`s (being the location of ants that should move and the
direction they should move in).

The simplest possible implementation is:

    def ordersFrom(gameState: Game) = Set.empty[Order]


## Status

This bot has been tested locally with data on standard input and with 
a locally running game engine. There may yet be undiscovered bugs within 
the game loop. Assume nothing. At any rate, all code in this 
starter package is able to be modified or removed within your own
Scala bot submission.

Patches to the Scala starter bot are welcomed. Fork the [offical repo](https://github.com/aichallenge/aichallenge)
if you wish. Or, fork [my fork](https://github.com/Synesso/aichallenge)
if you would like a review.


