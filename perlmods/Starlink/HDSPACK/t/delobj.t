#!perl -w

# Test of delobj function

use strict;
use Starlink::HDSPACK qw/ delobj /;
use vars qw/$loaded $NUM/;
BEGIN { $| = 1; print "1..3\n"; }
END {print "not ok 1\n" unless $loaded;}
$loaded = 1;
$NUM = 1;
&ok;

$Starlink::HDSPACK::DEBUG = 1;

# Get good status
my $status = &Starlink::HDSPACK::SAI__OK;
my $good = $status;



# Now need to set up a target
my $in = "testhds";
my $out = "out$$";
my $exstat = system( "cp  ${in}.sdf ${out}.sdf");
if ($exstat == 0) {
  &ok;
} else {
  &notok;
  die "No point continuing test\n";
}

$status = delobj("${out}.more.fits", $status);
&check_status($status);

unlink "${out}.sdf";

exit;

# Sub to check status
sub check_status {
  my $status = shift;
  if ($status == $good) {
    &ok; 
  } else {
    &notok;
  }
}

sub ok {
  print "ok $NUM\n";
  $NUM++;
}

sub notok {
  print "not ok $NUM\n";
  $NUM++;
}
