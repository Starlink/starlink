#!perl

# Simple test of versioning.
# Does not use any versions actually installed since
# the test can not guarantee a particular version of
# an app is installed or what version it may have.

use strict;
use Test;
BEGIN { plan tests => 7 }
use Starlink::Versions qw/ :Funcs /;

# Need to know where I am
use Cwd; 

# loaded ok
ok(1);

# The first thing to do is write out some version information
# as a version.dat file
open ( VERSION, "> version.dat") or die "Could not open version.dat: $!";

my $major = 1;
my $minor = 4;
my $patch = 17;
my $ver1 = "V$major.$minor-$patch";
print VERSION $ver1, "\n";
close VERSION or die "Could not close version.dat file";

ok(2); # Version file written

# Set PROG_DIR to the current directory
$ENV{THIS_PROG_DIR} = cwd;

# Now get the version
ok( starversion_major('this_prog'), $major);
ok( starversion_minor('this_prog'), $minor);
ok( starversion_patchlevel('this_prog'), $patch);
ok( starversion_string('this_prog'), $ver1);

# This will return undef
ok( !defined starversion_major('your_prog') );

unlink "version.dat";

