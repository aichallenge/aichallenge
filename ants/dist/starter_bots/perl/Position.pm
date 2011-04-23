package Position;
use strict;
use warnings;
use 5.10.0;
use feature 'switch';
use Carp qw(croak);

# Virtual class to store x/y coordinates

# Sadly there is no Class::Accessor-like module in Perl core :(

=head1 NAME

Position

=head1 SYNOPSIS

Objects containing position information (ie. row, column).

=head1 METHODS

=head2 new

Constructor - takes (x,y) params.

=cut

sub new {
    my ($class, $x, $y) = @_;
    bless {
        _x => $x,
        _y => $y,
    }, $class;
}

=head2 from

Alternative constructor - takes another Position object as parameter.

ie. It copies it into a new object.

=cut

sub from {
    my ($class, $source) = @_;
    bless {
        _x => $source->x,
        _y => $source->y,
    }, $class;
}

=head2 x

Returns x coordinate.

=cut

sub x {
    my ($self, $newval) = @_;
    if (defined $newval) {
        $self->{_x} = $newval;
    }
    return $_[0]->{_x}
}

=head2 y

Returns y coordinate.

=cut

sub y {
    my ($self, $newval) = @_;
    if (defined $newval) {
        $self->{_y} = $newval;
    }
    return $_[0]->{_y}
}

=head2 row

Returns row coordinate.

=cut

sub row {
    my ($self, $newval) = @_;
    $self->y($newval);
}

=head2 col

Returns column coordinate.

=cut

sub col {
    my ($self, $newval) = @_;
    $self->x($newval);
}

=head2 move

Adjusts the coordinates of this Position according to a given direction
(N, S, E or W)

=cut

sub move {
    my ($self, $direction) = @_;
    given ($direction) {
        when ('N') { $self->{_y}-- }
        when ('S') { $self->{_y}++ }
        when ('E') { $self->{_x}++ }
        when ('W') { $self->{_x}-- }
        default { croak("Invalid direction; $direction") }
    }
    return $self;
}

=head1 AUTHOR

Toby Corkindale, tjc@cpan.org.

=head1 LICENSE

This code is freely released into the public domain for use by maintainers and
entrants of the AI Challenge.

=cut

1;
