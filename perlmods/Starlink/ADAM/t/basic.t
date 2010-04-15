# Before `make install' is performed this script should be runnable with
# `make test'. After `make install' it should work as `perl test.pl'
#

# Basic test script to start and stop ADAM messaging


# Change 1..1 below to 1..last_test_to_print .
# (It may become useful if the test is moved to ./t subdirectory.)

BEGIN { $| = 1; print "1..3\n"; }
END {print "not ok 1\n" unless $loaded;}
use Starlink::ADAM;
$loaded = 1;
print "ok 1\n";

######################### End of black magic.


# Start ADAM

$status = adam_start "TEST$$";

if ($status == &Starlink::ADAM::SAI__OK) {
  print "ok 2\n";
} else {
  print "not ok 2\n";
}


# Shut down -- this croaks if it fails
adam_exit;
print "ok 3\n";

