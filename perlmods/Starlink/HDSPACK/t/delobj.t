#!perl -w

# Test of delobj function

use strict;
use Test;
BEGIN { plan tests => 5 }
use Starlink::HDSPACK qw/ delobj /;

$Starlink::HDSPACK::DEBUG = 1;

# Get good status
my $status = &Starlink::HDSPACK::SAI__OK;
my $good = $status;



# Now need to set up a target
my $in = "testhds";
my $out = "out$$";
my $exstat = system( "cp  ${in}.sdf ${out}.sdf")
  and die "Failed to copy in test NDF";
ok($exstat, 0);


$status = delobj("${out}.more.fits", $status);
ok($status, $good);

$status = delobj("${out}.more", $status);
ok($status, $good);

$status = delobj("${out}", $status);
ok($status, $good);

# File should now be gone
ok( !-e $out.".sdf");

exit;

