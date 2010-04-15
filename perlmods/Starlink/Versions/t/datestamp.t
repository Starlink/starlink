#!perl

# Fake a datestamp file and test the version reading

use strict;
use Test::More tests => 5;
use File::Spec;
use File::Path qw/ mkpath rmtree/;

BEGIN {
  use_ok( "Starlink::Versions" );
  Starlink::Versions->import( ":Funcs" );
}

# We need to set an environment variable which contains
# a Starlink type tree

my $root = File::Spec->catdir(File::Spec->curdir, 'star');

$ENV{THIS_PROG_DIR} = File::Spec->catdir(
					 $root,
					 'bin',
					 'this_prog'
					);

my $datedir = File::Spec->catdir(
				 $root,
				 'dates',
				);

mkpath( [$ENV{THIS_PROG_DIR}, $datedir])
  or die "Could not create skeleton Starlink tree";


# Set up tidy up block
END {
  rmtree( $root );
}

# Now create a datestamp file
# Use global filehandles for backwards compatibility with perl5.005
my $outfile = File::Spec->catfile($datedir,"this_prog_datestamp");
open DATESTAMP, "> $outfile" or
  die "Could not create test file: $!\n";

print DATESTAMP "Package : THIS_PROG\n";
print DATESTAMP "Version : V5.16-4\n";
print DATESTAMP "\nBuilt by: perl test program\n";
print DATESTAMP "\nPlease remove if found\n";

close(DATESTAMP) or die "Could not close datestamp file: $!\n";

ok(1, "Wrote datestamp file");

# Now get the version
my ($maj, $min, $patch) = starversion('this_prog');


is($maj, 5, "cf major");
is($min, 16, "cf minor");
is($patch, 4, "cf patch");
