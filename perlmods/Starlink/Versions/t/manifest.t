#!perl

# Fake a manifest file and test the version reading

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
				 'manifests',
				);

mkpath( [$ENV{THIS_PROG_DIR}, $datedir])
  or die "Could not create skeleton Starlink tree";


# Set up tidy up block
END {
  rmtree( $root );
}

# Now create a datestamp file
my $outfile = File::Spec->catfile($datedir,"this_prog");
# Use global filehandles for backwards compatibility with perl5.005
open MANIFEST, "> $outfile" or
  die "Could not create test file: $!\n";

print MANIFEST "<?xml version='1.0'?>\n";
print MANIFEST "<manifest component='starx'>\n";
print MANIFEST "<version>5.16-4</version>\n";
print MANIFEST "\nPlease remove if found\n";

close(MANIFEST) or die "Could not close manifest file: $!\n";

ok(1,"Wrote manifest file");

# Now get the version
my ($maj, $min, $patch) = starversion('this_prog');


is($maj, 5, "cf major");
is($min, 16, "cf minor");
is($patch, 4, "cf patch");
