# Before `make install' is performed this script should be runnable with
# `make test'. After `make install' it should work as `perl test.pl'

######################### We start with some black magic to print on failure.

# Change 1..1 below to 1..last_test_to_print .
# (It may become useful if the test is moved to ./t subdirectory.)

BEGIN { $| = 1; print "1..2\n"; }
END {print "not ok 1\n" unless $loaded;}
use Starlink::EMS;
$loaded = 1;
print "ok 1\n";

######################### End of black magic.

# Test the status handling by converting SAI__ERROR to 
# a code and back again

my $status = Starlink::EMS::SAI__ERROR;

$text = 0; # -w protection
ems1_get_facility_error($status, $fac, $ident, $text);

my $err = $fac . "__$ident";

if ($err eq 'SAI__ERROR') {
  print "ok 2\n";
} else {
  print "not ok 2\n";
}
