
# Test the tied interface

use strict;
use Test;

BEGIN { plan tests => 10 }

use GSD;

ok(1);

my $filename = "t/obs_das_0141";
my (@array, %hash);

# Tie to an array 

my $gsd = tie @array, 'GSD', $filename;

ok(ref($gsd), 'GSD');


ok( $array[2], 'PFS');
ok( $array[148]->[1], 0.15625);

# Tie to a hash

tie %hash, ref($gsd), $gsd;

ok( $hash{C3DAT}, 1994.082);
ok( $hash{C4RX}, 1800);
ok( $hash{C6XNP}, 1);
ok( $hash{C12SCAN_VARS2}->[0], 'SCAN_TIME');
ok( $hash{C12SCAN_TABLE_2}->[1], 4);
ok( sprintf("%4.2f",$hash{C12TASKY}->[1]), 59.79 );


