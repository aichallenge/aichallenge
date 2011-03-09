# Common Lisp Starter Package for Ant Wars

This file contains specific information about the Common Lisp (CL)
starter package for the [Ant Wars Google AI Challenge](http://ai-contest.com/).

It is assumed you are using [SBCL](http://www.sbcl.org/), since that
is what the challenge server will be using as well.


## Status

This bot is still in development and has only been tested locally with
data on standard input.  Nevertheless, patches and improvements are
welcome.


## Usage

Run `bin/run-ants-bot.sbcl` and paste the sample input from the
[Ants Game Specification](http://github.com/aichallenge/aichallenge/wiki/Ants-Game-Specification).

There are also a few initial unit tests which can be run by issueing
`bin/run-tests.sh`.


## Platforms

The code has so far only been tested on x86 using SBCL 1.0.45.debian.
