# Common Lisp Starter Package for Ant Wars

See the section after "Google AI Challenge Blurb" for information
specific to this Common Lisp starter package.


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


## Common Lisp Starter Package Specific Information

This rest of this file contains specific information about the Common
Lisp (CL) starter package for the
[Ant Wars Google AI Challenge](http://ai-contest.com/).

It is assumed you are using [SBCL](http://www.sbcl.org/), since that
is what the challenge server will be using as well.


## Status

This bot is still in development and has only been tested locally with
data on standard input.  Nevertheless, patches and improvements are
welcome.


## Usage

Run `bin/run-ants-bot.sbcl` and paste the sample input from the
[Ants Game Specification](http://github.com/aichallenge/aichallenge/wiki/Ants-Game-Specification).

There are also a few initial unit tests which can be run by issuing
`bin/run-tests.sh`.


## Platforms

The code has so far only been tested on x86 using SBCL 1.0.45.debian.
