#!perl

# Simple test of versioning.
# Does not use any versions actually installed since
# the test can not guarantee a particular version of
# an app is installed or what version it may have.

use strict;
use Test;
BEGIN { plan tests => 13 }
use Starlink::Versions qw/ :Funcs /;

# Need to know where I am
use Cwd; 

# loaded ok
ok(1);

# The first thing to do is write out some version information
# as a version.dat file
END { unlink "version.dat" if -e "version.dat" }
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

# Do some comparisons
ok( starversion_cmp('this_prog', $ver1), 0);
ok( starversion_cmp('this_prog', 'V5.6.7'), -1);
ok( starversion_cmp('this_prog', 'V1.2-3'), 1);

ok( starversion_lt('this_prog', '1.4-18') );
ok( starversion_eq('this_prog', '1.4-17') );
ok( starversion_gt('this_prog', '1.3-18') );

# This will return undef
ok( !defined starversion_major('your_prog') );

