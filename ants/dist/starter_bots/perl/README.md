# Perl Starter Bot

The Perl bot consists of a two parts.
Firstly, Ants.pm is a library of utility functions for interacting with the
game. Details of those functions are below.
Secondly, MyBot.pm is an example implementation of a working bot.

It contains two important methods: setup() and create_orders().

setup() will be called once, after the game parameters come in, but before the
game starts running.

create_orders() will be called once per game turn, and it should issue orders
via issue_order().


# ANTS LIBRARY METHODS

## new

The constructor; doesn't really take any parameters at the moment.

Must be called prior to any other methods, though.

## height

Returns height of the game board

## width

Returns width of the game board

## run

The primary processing loop. This will get orders and map data and then
call methods to generate orders, until the game ends.

## parse

Incoming data parser.

## finish_turn

Called to indicate the turn is over. Automatically called by run()

## issue_order

Method to issue an order to the server. Takes parameters of the x and y
coordinates of the ant, plus the direction for it to travel in. 
(Which must be N, E, S or W)

## map_search

Method to return a list of types-of-things from a given map.

Searches a map and returns an array of array-refs for occupied points.
Also takes a value to check against for those occupied points.
Returns like: ( [0,1], [10,21] )

## my_ants

Returns list of our ants on the map

Eg: ( [0,1], [10,21] )

## enemy_ants

Returns list of enemy ants on the map

Eg: ( [0,1], [10,21] )

## food

Returns list of visible food on the map

Eg: ( [2,5], [11,22] )

## water

Returns a list of known water tiles from the map.

Eg: ( [2,5], [11,22] )

## passable

Returns true/false indicating if given point is passable.. (ie. not water)
Note: Does not check for current occupation by other ants.

## distance

Calculate the distance between two points.

## direction

Given a current point (x1,y1), return which direction to move to get to
another point (x2,y2).
Note: Does not take into account impassable terrain.

## create_orders

This method is called each turn to generate orders. It does nothing here, and
should be extended in a subclass.

## setup

You may optionally override setup() in your own bot.

It is called after the initial configuration data is sent from the server.

# AUTHOR

Toby Corkindale, tjc@cpan.org.

# LICENSE

This code is freely released into the public domain for use by maintainers and
entrants of the AI Challenge.
