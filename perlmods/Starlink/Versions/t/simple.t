#!perl

# Simple test of versioning.
# Does not use any versions actually installed since
# the test can not guarantee a particular version of
# an app is installed or what version it may have.

use strict;
use Test::More tests => 53;

# Need to know where I am
use File::Spec;

BEGIN {
  use_ok( "Starlink::Versions" );
  Starlink::Versions->import( ":Funcs" );
}

# The first thing to do is write out some version information
# as a version.dat file
END { unlink "version.dat" if -e "version.dat" }

my $major = 1;
my $minor = 4;
my $patch = 17;

my $verstr = write_version_file( $major, $minor, $patch);
ok(defined $verstr, "Got version string from write function");

# Set PROG_DIR to the current directory
$ENV{THIS_PROG_DIR} = File::Spec->curdir;

# Now get the version
is( starversion_major('this_prog'), $major, "cf major");
is( starversion_minor('this_prog'), $minor, "cf minor");
is( starversion_patchlevel('this_prog'), $patch, "cf patch");
is( starversion_string('this_prog'), $verstr, "cf string");

# Do some comparisons
is( starversion_cmp('this_prog', $verstr), 0);
is( starversion_cmp('this_prog', 'V5.6.7'), -1);
is( starversion_cmp('this_prog', 'V1.2-3'), 1);

ok( starversion_lt('this_prog', '1.4-18') );
ok( starversion_eq('this_prog', '1.4-17') );
ok( starversion_gt('this_prog', '1.3-18') );
ok( starversion_le('this_prog', '1.4-17') );
ok( starversion_le('this_prog', '1.4-18') );
ok( starversion_ge('this_prog', '1.4-17') );
ok( starversion_ge('this_prog', '1.3-17') );

# This will return undef
ok( !defined starversion_major('your_prog'), "Test unknown program" );

# Test the parsing
my @vers = ( [1,4,17], [1,4,0], [1,4,undef]);

for my $v (@vers) {
  for my $nov (1,0) {
    for my $usedot (1,0) {
      my $vstr = write_version_file( @$v, $nov, $usedot);
      print "# $vstr\n";
      is( starversion_major('this_prog'), $major, "cf major");
      is( starversion_minor('this_prog'), $minor, "cf minor");
      is( starversion_patchlevel('this_prog'), $patch, "cf patch");
    }
  }
}


exit;

sub write_version_file {
  my ($maj, $min, $pt, $no_v, $usedot) = @_;

  open ( my $VERSION, "> version.dat") or die "Could not open version.dat: $!";

  my $v= ( $no_v ? '' : 'V');
  my $d= ( $usedot ? '.' : '-' );

  # check for undef patch
  if (!defined $pt) {
    $d = '';
    $pt = '';
  }

  # form the string and write it
  my $verstr = "$v$maj.$min$d$pt";
  print $VERSION $verstr, "\n";
  close $VERSION or die "Could not close version.dat file";
  return $verstr;
}

