
# Test the object interface

use strict;
use Test;

BEGIN { plan tests => 13 }

use GSD;

ok(1);

my $filename = "t/obs_das_0141";

my $gsd = new GSD( $filename );

ok(defined $gsd);

ok(sprintf("%4.2f", $gsd->version), '5.30');
ok($gsd->nitems, 167);

# Get some scalars

ok( $gsd->GetByName('C4LSC'), 'AZ' );

ok( $gsd->GetByNum(2), 'm94bu22' );

# Get an array

my @data = $gsd->GetByName('C12SCAN_VARS1');
ok($data[0], 'LST');
ok($data[1], 'AIRMASS');

@data = $gsd->GetByName('C12GS');
ok($data[0], 0.5);

# Test the InqSize function

my ($dimnm, $dimunt, $dimvals, $size) = $gsd->InqSize(133);

ok($dimnm->[0], 'C3NPP');
ok($dimnm->[1], 'C3NMAP');
ok($dimvals->[0], 2);
ok($dimvals->[1], 1);
