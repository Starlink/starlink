#!/usr/local/bin/perl -w

# Basic test of tokens

use strict;
use Test;
BEGIN { plan tests => 4 }
use Starlink::EMS;

my $ok = SAI__OK;
my $status = $ok;

# Start ems
ems_begin( $status );
ok($status, $ok);

# Copy some values into some tokens and retrieve them
ems_setl('LOG', 1);
my $mstr = emsExpnd("^LOG", $status);

ok($mstr, 'TRUE');

ems_setc('FLT', '3.141592654');
$mstr = emsExpnd('^FLT', $status);

ok($mstr, '3.141592654');

# End ems
ems_end( $status );

ok($status, $ok);
