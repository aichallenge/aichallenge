# AI Challenge Source Code Repository

_Codename: Epsilon_

This code provides the basis for the Fall 2011 [AI Challenge](http://aichallenge.org/).
Epsilon is supposed to implement the ants game.

## To fix

- [ ] Support opponent.sql

## Setup

1. First run `python setup.py` in the root folder. This will create the starter bots in `ants/dist/starter_bots` and copy them to `website/starter_packages/` where the user can later download them.
   It will then download the wiki files, which are parsed and put into the php files.

## Folder Contents

- `ants/` - Everything related to ants: engine, starter packages, maps/mapgen, visualizer
- `integration_testing` - Anything related to testing/stress-testing the contest setup
- `manager/` - Tournament manager which coordinates the workers and computes skill ratings
- `worker/` - Standalone workers which run games (including compiler and sandbox)
- `sql/` - Files for creating an empty sql database
- `website/` - Main website and frontend

Initializing the git submodules:

- `git submodule init`
- `git submodule update`

## Installation

Follow [INSTALL.md](https://github.com/aichallenge/aichallenge/blob/epsilon/INSTALL.md).
