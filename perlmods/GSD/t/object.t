#!perl
# Test the object interface

use strict;
use Test::More tests => 13;

BEGIN { use_ok( "GSD" ); }

my $filename = "t/obs_das_0141";

my $gsd = new GSD( $filename );

ok(defined $gsd,  "Got object");

is(sprintf("%4.2f", $gsd->version), '5.30', "Check version");
is($gsd->nitems, 167, "Check number of items");

# Get some scalars

is( $gsd->GetByName('C4LSC'), 'AZ', "Read C4LSC" );

is( $gsd->GetByNum(2), 'm94bu22', "Read item 2" );

# Get an array

my @data = $gsd->GetByName('C12SCAN_VARS1');
is($data[0], 'LST', "First element from C12SCAN_VARS1");
is($data[1], 'AIRMASS', "Second element from C12SCAN_VARS1");

@data = $gsd->GetByName('C12GS');
is($data[0], 0.5, "Read C12GS element 1");

# Test the InqSize function

my ($dimnm, $dimunt, $dimvals, $size) = $gsd->InqSize(133);

is($dimnm->[0], 'C3NPP', "Name of dim 0");
is($dimnm->[1], 'C3NMAP', "Name of dim 1");
is($dimvals->[0], 2, "Dimval[0]");
is($dimvals->[1], 1, "Dimvals[1]");
