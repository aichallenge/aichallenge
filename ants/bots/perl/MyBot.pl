#!/usr/bin/env perl
use strict;
use warnings;

# These two lines may not turn out to be neccessary, depending upon the
# final game environment..
use FindBin;
use lib "$FindBin::Bin";

# The implementation of your bot exists in MyBot.pm
use MyBot;
MyBot->new->run;

1;
