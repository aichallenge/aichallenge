package MyBot;
use strict;
use warnings;
use parent 'Ants';

# This is where the implementation of your bot goes!

# setup() will be called once, after the game parameters come in, but before
# the game starts up.
sub setup {
    my $self = shift;
}

# create_orders() will be run after parsing the incoming map data, every turn.
sub create_orders {
    my $self = shift;

    for my $ant ($self->my_ants) {
        my ($antx, $anty) = @$ant;
        my $food = $self->nearby_food($ant);
        my $direction;

        if ($food) {
            my ($foodx, $foody) = @$food;
            $direction = $self->direction(
                $antx, $anty,
                $foodx, $foody
            );
        }
        else {
            if ($self->passable($antx+1, $anty)) {
                $direction = 'E'
            }
            elsif ($self->passable($antx, $anty+1)) {
                $direction = 'N'
            }
            elsif ($self->passable($antx-1, $anty)) {
                $direction = 'W'
            }
            elsif ($self->passable($antx, $anty-1)) {
                $direction = 'S'
            }
        }

        $self->issue_order(
            $antx, $anty, $direction
        );
    }
}

sub nearby_food {
    my ($self, $ant_location) = @_;

    my $goal_distance = ($self->width + $self->height) / 2;

    for my $food ($self->food) {
        if (
            $self->distance(
                $ant_location->[0], $ant_location->[1],
                $food->[0], $food->[1]
            ) < $goal_distance
        ) {
            return $food;
        }
    }
    return;
}

1;
