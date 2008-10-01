#!perl -w

use Test::More tests => 17;
use strict;

use_ok( "NDF" );

# ================================================================
#   Test ERR calls
#
# ================================================================

my $indf;

# initialise global status
my $status = &NDF::SAI__OK;
my $good   = $status;

# Start the error context
err_begin($status);

# Make an error and annul

$status = &NDF::SAI__ERROR;
err_rep("","An artificial error", $status);
err_annul($status);
is($status, $good, "Check status");


# Test  generates an error from NDF - so that status is bad

err_mark();
ndf_find('junk_loc', 'test', $indf, $status);
err_level( my $lev);
is($lev,3, "Check level");
err_rlse();
err_level( $lev );
is($lev, 2, "check level");

ok( $status != $good, "Status now bad");

# Test 2 flush the status and end context

print "# This error message is good:\n";
err_flush($status);
is($status, $good, "Check status");

# Tuning - generate output even from annul
err_tune('REVEAL', 1, $status);

$status = &NDF::SAI__WARN;
err_rep("","Hidden output from TUNE should be visible", $status);
err_annul($status);
is($status, $good, "check status");

$ENV{ERR_REVEAL} = 0;
err_tune('REVEAL', 0, $status);

# Try to retrieve without any messages
my ($param, $plen, $opstr, $oplen);
err_load($param, $plen, $opstr, $oplen, $status);
is($status, &NDF::EMS__NOMSG, "Check NOMSG status");
err_load($param, $plen, $opstr, $oplen, $status); # Run it again to clear it
err_annul($status);
is($status, $good,"Check status");



# Retrieve a line at a time via err_load
$status = &NDF::SAI__WARN;
err_rep("PAR1","Line 1", $status);
err_rep("PAR2","Line 2", $status);

err_load($param, $plen, $opstr, $oplen, $status);
is($param, "PAR1","Check param 1");
is($opstr, "Line 1", "Line 1");
err_load($param, $plen, $opstr, $oplen, $status);
is($param, "PAR2", "Check param 2");
is($opstr, "Line 2", "Line 2");
err_load($param, $plen, $opstr, $oplen, $status);
is($status, $good, "Check status");

# Retrieve last status - should be good
err_stat(my $i);
is($i, $good, "check status");

# Token tests (cant check the return)
err_facer('TESTING', &NDF::ERR__BADOK);


err_end($status);
# Status should be good here
is($status, $good, "Check status");

err_level($lev);
is($lev, 1,"level now 1");

