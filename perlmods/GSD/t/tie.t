#!perl
# Test the tied interface

use strict;
use Test::More tests => 10;

BEGIN { use_ok( "GSD" ); }

my $filename = "t/obs_das_0141";
my (@array, %hash);

# Tie to an array 

my $gsd = tie @array, 'GSD', $filename;

isa_ok($gsd, 'GSD', "GSD object");


is( $array[2], 'PFS', "Check element 3 of item 2");
is( $array[148]->[1], 0.15625, "Check element 2 of item 148");

# Tie to a hash

tie %hash, ref($gsd), $gsd;

is( $hash{C3DAT}, 1994.082, "C3DAT");
is( $hash{C4RX}, 1800, "C4RX");
is( $hash{C6XNP}, 1, "C6XNP");
is( $hash{C12SCAN_VARS2}->[0], 'SCAN_TIME', "C12SCAN_VARS2");
is( $hash{C12SCAN_TABLE_2}->[1], 4, "C12SCAN_TABLE_2");
is( sprintf("%4.2f",$hash{C12TASKY}->[1]), 59.79, "C12TASKY" );


