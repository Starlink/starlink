# Before `make install' is performed this script should be runnable with
# `make test'. After `make install' it should work as `perl test.pl'

######################### We start with some black magic to print on failure.

# Change 1..1 below to 1..last_test_to_print .
# (It may become useful if the test is moved to ./t subdirectory.)

BEGIN { $| = 1; print "1..6\n"; }
END {print "not ok 1\n" unless $loaded;}
use Starlink::NBS;
$loaded = 1;
print "ok 1\n";

######################### End of black magic.

# Insert your test code below (better if it prints "ok 13"
# (correspondingly "not ok 13") depending on the success of chunk 13
# of the test code):

# Find the NBS (from an already running CGS4DR so we need a pid as well!)

$name = "p57321_plotnb";

$nbs = new Starlink::NBS("$name");
&isok(2, $nbs->status);


$hist = $nbs->find(".port_0.histogram_bins");
&isok(3, $hist->status);


($status, $value) = $hist->get;
print "Value was $value\n";
&isok(4, $status);

$disp = $nbs->find(".port_0.display_data");
($status, $val) = $disp->get;
print "Value was $val\n";
&isok(5, $status);


# Now put in a string
$status = $disp->put("pid=$$");
($status, $now) = $disp->get;
print "Value is $now\n";
&isok(6, $status);


# Subroutine to print ok, notok

sub isok {

  my ($num, $val) = @_;

  if ($val == 0) {
    print "ok $num\n";
  } else {
    print "not ok $num\n";
  }

}
