package Ants;
use strict;
use warnings;
use feature ':5.10';
use List::Util qw(first max min reduce shuffle sum);

# Constructor
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

# Returns height of the game board
sub height {
    my $self = shift;
    return $self->{config}{rows};
}

# Returns width of the game board
sub width {
    my $self = shift;
    return $self->{config}{cols};
}

# Primary loop.
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

# Incoming data parser
sub parse {
    my $self = shift;
    # Reset maps:
    $self->{food} = $self->{water} = $self->{corpses} = $self->{ants} = undef;

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

sub finish_turn { say "go" }

sub issue_order {
    my ($self, $x, $y, $direction) = @_;
    # direction must be N E S or W
    say "o $y $x $direction";
}

# Searches a map and returns an array of array-refs for occupied points.
# Also takes a value to check against for those occupied points.
# Returns like: ( [0,1], [10,21] )
sub map_search {
    my ($self, $map, $value) = @_;
    my @points;
    for my $y (0 .. $self->height) {
        for my $x (0 .. $self->width) {
            if (exists $map->{$x}{$y}
                and $map->{$x}{$y} == $value
            ) {
                push(@points, [$x,$y])
            }
        }
    }
    return @points;
}

# Returns list of our ants on the map
# Eg: ( [0,1], [10,21] )
sub my_ants {
    my $self = shift;
    return $self->map_search($self->{ants}, 0);
}

# Returns list of visible food on the map
# Eg: ( [2,5], [11,22] )
sub food {
    my $self = shift;
    return $self->map_search($self->{food}, 1);
}

# Returns true/false indicating if given point is passable.. (ie. not water)
# Note: Does not check for current occupation by other ants.
sub passable {
    my ($self, $x, $y) = @_;
    return(not $self->{water}->{$x}{$y});
}

# Calculate the distance between two points
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

# Given a current point (x1,y1), return which direction to move to get to
# another point (x2,y2).
# Note: Does not take into account impassable terrain.
sub direction {
    my ($self, $x1, $y1, $x2, $y2) = @_;
    if ($x1 < $x2) { return 'E' }
    if ($x1 > $x2) { return 'W' }
    if ($y1 < $y2) { return 'N' }
    if ($y1 > $y2) { return 'S' }
    return ''
}

sub create_orders {
    my $self = shift;
    die "You must override create_orders() in your own bot.";
}

sub setup {
    my $self = shift;
    # You may optionally override setup() in your own bot.
    # It is called after the initial configuration data is sent from the
    # server.
}

1;
