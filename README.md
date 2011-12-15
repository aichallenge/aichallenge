AI Challenge Source Code Repository
===================================

*Codename: Epsilon*

This code provides the basis for the Fall 2011 [AI Challenge](http://aichallenge.org/).
Epsilon is supposed to implement the ants game.

Folder Contents
---------------

* `ants/` - Everything related to ants: engine, starter packages, maps/mapgen, visualizer
* `integration_testing` - Anything related to testing/stress-testing the contest setup
* `manager/` - Tournament manager which coordinates the workers and computes skill ratings
* `worker/` - Standalone workers which run games (including compiler and sandbox)
* `sql/` - Files for creating an empty sql database
* `website/` - Main website and frontend

Initializing the git submodules:

* `git submodule init`
* `git submodule update`

Installation
---------------

Follow [INSTALL.md](https://github.com/aichallenge/aichallenge/blob/epsilon/INSTALL.md).