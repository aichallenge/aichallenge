package MyBot;
use strict;
use warnings;
use parent 'Ants';
use Position;

# This is where the implementation of your bot goes!

# setup() will be called once, after the game parameters come in, but before
# the game starts up.
sub setup {
    my $self = shift;
}

# create_orders() will be run after parsing the incoming map data, every turn.
# This example looks for nearby food that isn't immediately blocked by water.
sub create_orders {
    my $self = shift;

    for my $ant ($self->my_ants) {
        my @food = $self->nearby_food($ant);

        for my $f (@food) {
            my $direction = $self->direction($ant, $f);
            next unless $self->passable(
                Position->from($ant)->move($direction)
            );
            $self->issue_order( $ant, $direction );
            last;
        }

    }
}

# This demonstrates the use of some of the methods available in the Ants
# library.
# This subroutine returns a list of nearby food for a given location, sorted
# by distance.
sub nearby_food {
    my ($self, $ant_location) = @_;

    my @foods;
    for my $food ($self->food) {
        my $dist = $self->distance( $ant_location, $food);
        push @foods, { distance => $dist, tile => $food };
    }

    # Sort by distance:
    return map { $_->{tile} } sort {
        $a->{distance} <=> $b->{distance}
    } @foods;
}

1;
