#!perl -w

use strict;
use Test;
BEGIN { plan tests => 16 }

use NDF;

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
ok($status, $good);


# Test  generates an error from NDF - so that status is bad

err_mark;
ndf_find('junk_loc', 'test', $indf, $status);
err_level( my $lev);
ok($lev,3);
err_rlse;
err_level( $lev );
ok($lev, 2);

ok( $status != $good);

# Test 2 flush the status and end context

print "# This error message is good:\n";
err_flush($status);
ok($status, $good);

# Tuning - generate output even from annul
err_tune('REVEAL', 1, $status);

$status = &NDF::SAI__WARN;
err_rep("","Hidden output from TUNE should be visible", $status);
err_annul($status);
ok($status, $good);

$ENV{ERR_REVEAL} = 0;
err_tune('REVEAL', 0, $status);

# Try to retrieve without any messages
my ($param, $plen, $opstr, $oplen);
err_load($param, $plen, $opstr, $oplen, $status);
ok($status, &NDF::EMS__NOMSG);
err_load($param, $plen, $opstr, $oplen, $status); # Run it again to clear it
err_annul($status);
ok($status, $good);



# Retrieve a line at a time via err_load
$status = &NDF::SAI__WARN;
err_rep("PAR1","Line 1", $status);
err_rep("PAR2","Line 2", $status);

err_load($param, $plen, $opstr, $oplen, $status);
ok($param, "PAR1");
ok($opstr, "Line 1");
err_load($param, $plen, $opstr, $oplen, $status);
ok($param, "PAR2");
ok($opstr, "Line 2");
err_load($param, $plen, $opstr, $oplen, $status);
ok($status, $good);

# Retrieve last status - should be good
err_stat(my $i);
ok($i, $good);

# Token tests (cant check the return)
err_facer('TESTING', &NDF::ERR__BADOK);
err_fioer('FIOERR', 52);


err_end($status);
# Status should be good here
ok($status, $good);

err_level($lev);
ok($lev, 1);

