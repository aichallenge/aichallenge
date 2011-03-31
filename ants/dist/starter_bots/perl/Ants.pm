package Ants;
use strict;
use warnings;
use 5.10.0;
use feature ':5.10';
use List::Util qw(first max min reduce shuffle sum);

=head1 NAME

Ants

=head1 SYNOPSIS

  Functions for interacting with the AI Contest's "Ants" game.

=head1 METHODS

=head2 new

The constructor; doesn't really take any parameters at the moment.

Must be called prior to any other methods, though.

=cut

sub new {
    my ($class, $args) = @_;

    # Ensure we're autoflushing output:
    $| = 1;

    my $self = {
        food => undef,
        water => undef,
        corpses => undef,
        ants => undef,
        config => undef,
    };

    bless $self, $class;
}

=head2 height

Returns height of the game board

=cut

sub height {
    my $self = shift;
    return $self->{config}{rows};
}

=head2 width

Returns width of the game board

=cut

sub width {
    my $self = shift;
    return $self->{config}{cols};
}

=head2 run

The primary processing loop. This will get orders and map data and then
call methods to generate orders, until the game ends.

=cut

sub run {
    my ($self) = @_;

    while (1) {
        my $mode = $self->parse;
        given ($mode) {
            when ('ready') {
                # Do nothing; Could do some setup here if you liked though.
                $self->setup;
            }
            when ('end') {
                # Game over.
                exit;
            }
            when ('go') {
                $self->create_orders;
            }
        }
        $self->finish_turn;
    }
}

=head2 parse

Incoming data parser.

=cut

sub parse {
    my $self = shift;
    # Reset maps:
    $self->{food} = $self->{corpses} = $self->{ants} = undef;

    while (1) {
        my $line = <>;
        next unless $line;
        my ($command, @args) = split(/\s/, $line);
        given ($command) {
            when ([qw(
                    loadtime
                    turntime
                    rows
                    cols
                    turn
                    turns
                    viewradius2
                    attackradius2
                    spawnradius2
            )]) {
                $self->{config}{$command} = shift @args;
            }
            when ('f') {
                $self->{food}{$args[1]}{$args[0]} = 1;
            }
            when ('w') {
                $self->{water}{$args[1]}{$args[0]} = 1;
            }
            when ('a') {
                $self->{ants}{$args[1]}{$args[0]} = $args[2];
            }
            when ('d') {
                $self->{corpses}{$args[1]}{$args[0]} = $args[2];
            }
            when ([qw(ready go end)]) { return $command }
        }
    }
}

=head2 finish_turn

Called to indicate the turn is over. Automatically called by run()

=cut

sub finish_turn { say "go" }

=head2 issue_order

Method to issue an order to the server. Takes parameters of the x and y
coordinates of the ant, plus the direction for it to travel in. 
(Which must be N, E, S or W)

=cut

sub issue_order {
    my ($self, $x, $y, $direction) = @_;
    say "o $y $x $direction";
}

=head2 map_search

Method to return a list of types-of-things from a given map.

Searches a map and returns an array of array-refs for occupied points.
Also takes a value to check against for those occupied points.
Returns like: ( [0,1], [10,21] )

=cut

sub map_search {
    my ($self, $map, $value) = @_;
    my @points;
    for my $y (0 .. $self->height) {
        for my $x (0 .. $self->width) {
            if (exists $map->{$x}{$y}
                and $map->{$x}{$y} ~~ $value
            ) {
                push(@points, [$x,$y])
            }
        }
    }
    return @points;
}

=head2 my_ants

Returns list of our ants on the map

Eg: ( [0,1], [10,21] )

=cut

sub my_ants {
    my $self = shift;
    return $self->map_search($self->{ants}, 0);
}

=head2 enemy_ants

Returns list of enemy ants on the map

Eg: ( [0,1], [10,21] )

=cut

sub enemy_ants {
    my $self = shift;
    return $self->map_search($self->{ants}, [1..3]);
}

=head2 food

Returns list of visible food on the map

Eg: ( [2,5], [11,22] )

=cut

sub food {
    my $self = shift;
    return $self->map_search($self->{food}, 1);
}

=head2 water

Returns a list of known water tiles from the map.

Eg: ( [2,5], [11,22] )

=cut

sub water {
    my $self = shift;
    return $self->map_search($self->{water}, 1);
}

=head2 passable

Returns true/false indicating if given point is passable.. (ie. not water)
Note: Does not check for current occupation by other ants.

=cut

sub passable {
    my ($self, $x, $y) = @_;
    return(not $self->{water}->{$x}{$y});
}

=head2 distance

Calculate the distance between two points.

=cut

sub distance {
    my ($self, $x1, $y1, $x2, $y2) = @_;
    my $dx = min(
        abs($x1 - $x2),
        $self->width - abs($x1 - $x2)
    );
    my $dy = min(
        abs($y1 - $y2),
        $self->height - abs($y1 - $y2)
    );
    return $dx + $dy;
}

=head2 direction

Given a current point (x1,y1), return which direction to move to get to
another point (x2,y2).
Note: Does not take into account impassable terrain.

=cut

sub direction {
    my ($self, $x1, $y1, $x2, $y2) = @_;
    if ($x1 < $x2) { return 'E' }
    if ($x1 > $x2) { return 'W' }
    if ($y1 < $y2) { return 'N' }
    if ($y1 > $y2) { return 'S' }
    return ''
}

=head2 create_orders

This method is called each turn to generate orders. It does nothing here, and
should be extended in a subclass.

=cut

sub create_orders {
    my $self = shift;
    die "You must override create_orders() in your own bot.";
}

=head2 setup

You may optionally override setup() in your own bot.

It is called after the initial configuration data is sent from the server.

=cut

sub setup {
    my $self = shift;
}

=head1 AUTHOR

Toby Corkindale, tjc@cpan.org.

=head1 LICENSE

This code is freely released into the public domain for use by maintainers and
entrants of the AI Challenge.

=cut

1;
