#!/usr/local/bin/perl -w

# Test deprecated routines
#   : emsMload

use strict;
use Test;
BEGIN { plan tests => 3 }
use Starlink::EMS qw/ emsMload emsBegin emsSetc emsEnd/;

my $ok = &Starlink::EMS::SAI__OK;
my $status = $ok;

# Start ems
emsBegin( $status );
ok($status, $ok);

# Copy some values into some tokens and retrieve them
emsSetc('TEST', "Hello");
my $mstr = emsMload("","^TEST", $status);

ok($mstr, 'Hello');

# End ems
emsEnd( $status );

ok($status, $ok);
