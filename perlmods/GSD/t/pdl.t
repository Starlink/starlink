
# Test the PDL subclassing

use strict;
use Test;

BEGIN { plan tests => 10 }

eval { require GSD::PDL; 1; };
if ($@) {
  foreach (1..10) {
    skip("Skip PDL not installed. Skipping",$_);
  }
  exit;
}

ok(1);

my $filename = "t/obs_das_0141";

my $gsd = new GSD::PDL( $filename );

ok(defined $gsd);

ok(sprintf("%4.2f", $gsd->version), '5.30');
ok($gsd->nitems, 167);

# Get some scalars

ok( $gsd->GetByName('C4LSC'), 'AZ' );

ok( $gsd->GetByNum(2), 'm94bu22' );

# Get an PDL array

my $data = $gsd->GetByName('C7VRADIAL');

ok( UNIVERSAL::isa($data, 'PDL'));

ok( $data->at(2), 49583.9539769676);

# A 2-d PDL array

$data = $gsd->GetByName('C11PHA');

ok( $data->at(0,1), 1800);

# A 3d array

$data = $gsd->GetByNum($gsd->nitems);

ok( $data->at(3,0,0), 9999);
