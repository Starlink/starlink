#!perl

# Test of 5.6.0 versioning using the new unicode string
# concatenation syntax

use strict;
use Test::More tests => 9;
use File::Spec;

BEGIN {
  use_ok( "Starlink::Versions" );
  Starlink::Versions->import( ":Funcs" );
}

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
close(VERSION) or die "Could not close version.dat file";

ok(1, "Wrote version file"); # Version file written

# Set PROG_DIR to the current directory
$ENV{THIS_PROG_DIR} = File::Spec->curdir;

########### END OF COPY ########################

my $version = starversion('this_prog');

is($version, version->new("5.1.17"));

ok($version gt version->new("0.15.2"));
ok($version lt version->new("20.51.1"));

ok($version le version->new("6.2.10"));
ok($version le version->new("5.1.17"));

ok($version ge version->new("0.12.34"));
ok($version ge version->new("5.1.17"));
