#!perl -w

use Test::More tests => 5;
use strict;
use_ok("NDF");

# ================================================================
#   Test NDF calls to read extension information
#   from test.sdf
# ================================================================

# Test file
my $file = "test";

# initialise global status
my $status = &NDF::SAI__OK;

# Initialise NDF
ndf_begin();

# Open up the test file
die "Couldn't find test file: $file\n" unless (-e "$file.sdf");

ndf_find(&NDF::DAT__ROOT, $file, my $indf, $status);

# How many extensions
my $num = 0;
ndf_xnumb($indf, $num, $status);

is($status, &NDF::SAI__OK, "Check status");
is($num, 3, "Check count");

# Read in a value
my $tcold = -1;
ndf_xgt0r($indf, 'REDS','T_COLD',$tcold, $status);

is($status, &NDF::SAI__OK, "Check status");
is($tcold, 55, "Check T_COLD");


my $sub = 'n/a';
ndf_xgt0c($indf, 'REDS','SUB_INSTRUMENT',$sub,$status);

is($status, &NDF::SAI__OK, "Check status");
is($sub, "LONG", "Check SUB_INSTRUMENT");

# Clean up and close the file

ndf_annul($indf, $status);
ndf_end($status);

is($status, &NDF::SAI__OK, "Check status");


