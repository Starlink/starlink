#!perl -w

use strict;
no strict "vars";

use NDF;

# ================================================================
#   Test ERR calls
#   
# ================================================================

$n=2; # number of tests
print "1..$n\n";

$indf = 0; #keep -w happy

# initialise global status
$status = &NDF::SAI__OK;

# Start the error context
err_begin($status);

# Make an error

# Test 1 generates an error - so that status is bad

err_mark;
ndf_find('junk_loc', 'test', $indf, $status);
err_rlse;

($status != &NDF::SAI__OK) &&
  (print "ok\n") || (print "not ok\n");

# Test 2 flush the status and end context

print "This error message is good:\n";
err_flush($status);

err_end($status);

($status == &NDF::SAI__OK) &&
  (print "ok\n") || (print "not ok\n");


