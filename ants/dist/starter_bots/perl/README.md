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

To get started, edit MyBot.pm; you can run it with: perl MyBot.pl

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

Method to issue an order to the server. Takes parameters of the Position
of the ant, plus the direction for it to travel in.
(Which must be N, E, S or W)

## map_search

Method to return a list of types-of-things from a given map.

Searches a map and returns an array of Position objects for occupied points.
Also takes a value to check against for those occupied points.

(Position objects encapsulate the row/col values)

## my_ants

Returns list of our ants on the map.

(As an array of Position objects)

Example:

  my @ants = $self->my_ants;
  for my $ant (@ants) {
    say "I am at " . $ant->row . " by " . $ant->col;
    say sprintf('Also known as (%d,%d)', $ant->x, $ant->y);
  }

## enemy_ants

Returns list of enemy ants on the map.

See my_ants()

## food

Returns list of visible food on the map

Data returned in same format as my_ants()

## water

Returns a list of known water tiles from the map.

Data returned in same format as my_ants()

## passable

Returns true/false indicating if given Position is passable.. (ie. not water)

Note: Does not check for current occupation by other ants.

## distance

Calculate the distance between two Positions.

## direction

Given a current Position, return which direction to move to get to
another Position.

Note: Does not take into account impassable terrain.
This is a fairly naive algorithm.

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

