#!perl

# Test of 5.6.0 versioning using the new unicode string
# concatenation syntax

use strict;
use Test;

BEGIN {plan tests => 9}
use Starlink::Versions qw/ starversion /;
use Cwd;

my $skip = ( $] < 5.006 ? "Skip perl version too old. Need 5.6.0" : 0);

ok(1);

########### COPIED FROM SIMPLE.T #################

# The first thing to do is write out some version information
# as a version.dat file
END { unlink "version.dat" if -e "version.dat" }
open ( VERSION, "> version.dat") or die "Could not open version.dat: $!";

my $major = 5;
my $minor = 1;
my $patch = 17;
my $ver1 = "V$major.$minor-$patch";
print VERSION $ver1, "\n";
close VERSION or die "Could not close version.dat file";

ok(2); # Version file written

# Set PROG_DIR to the current directory
$ENV{THIS_PROG_DIR} = cwd;

########### END OF COPY ########################

my $version = starversion('this_prog');

# Need to protect from old perls and simply skip the tests
# Need to turn off strict subs and undefined warnings
# Simpler to fake the tests
if ($skip) {
  skip($skip,1);
  skip($skip,2);
  skip($skip,3);
  skip($skip,4);
  skip($skip,5);
  skip($skip,6);
  skip($skip,7);
} else {
  no strict qw/subs/;
  skip($skip, $version, v5.1.17);

  skip($skip, ($version gt v0.15.2));
  skip($skip, ($version lt v20.51.1));

  skip($skip, ($version le v6.2.10));
  skip($skip, ($version le v5.1.17));

  skip($skip, ($version ge v0.12.34));
  skip($skip, ($version ge v5.1.17));
}
