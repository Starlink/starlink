#!/usr/local/bin/perl -w

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
my $mstr = ems_mload("","^LOG", $status);

ok($mstr, 'TRUE');

ems_fmtr('FLT', 'F5.2', 3.141592654);
$mstr = ems_mload("",'^FLT', $status);

ok($mstr, ' 3.14');

# End ems
ems_end( $status );

ok($status, $ok);
