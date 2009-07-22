#!perl
# Test the PDL subclassing

use strict;
use Test::More;

my $loaded = eval { require PDL::LiteF; 1; };
if (!$loaded) {
    print "***********************\n";
    plan skip_all => "PDL not installed";
} else {
    plan tests => 10;
}

use_ok( "GSD::PDL" );

my $filename = "t/obs_das_0141";

my $gsd = new GSD::PDL( $filename );

isa_ok($gsd, "GSD::PDL");

is(sprintf("%4.2f", $gsd->version), '5.30', "Version check");
is($gsd->nitems, 167, "Count items");

# Get some scalars

is( $gsd->GetByName('C4LSC'), 'AZ', "Check C4LSC" );

is( $gsd->GetByNum(2), 'm94bu22', "Check item 2" );

# Get an PDL array

my $data = $gsd->GetByName('C7VRADIAL');

isa_ok( $data, "PDL" );

is( $data->at(2), 49583.9539769676, "Check C7VRADIAL");

# A 2-d PDL array

$data = $gsd->GetByName('C11PHA');

is( $data->at(0,1), 1800, "Check C11PHA");

# A 3d array

$data = $gsd->GetByNum($gsd->nitems);

is( $data->at(3,0,0), 9999, "Get last item");
