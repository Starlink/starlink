#!perl

# Fake a datestamp file and test the version reading

use strict;
use Test;
BEGIN { plan tests => 5 }
use Starlink::Versions qw/ starversion /;
use Cwd;
use File::Spec;
use File::Path qw/ mkpath rmtree/;

ok(1);


# We need to set an environment variable which contains
# a Starlink type tree

my $root = File::Spec->catdir(cwd, 'star');

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
my $outfile = File::Spec->catfile($datedir,"this_prog_datestamp");
open DATESTAMP, "> $outfile" or 
  die "Could not create test file: $!\n";

print DATESTAMP "Package : THIS_PROG\n";
print DATESTAMP "Version : V5.16-4\n";
print DATESTAMP "\nBuilt by: perl test program\n";
print DATESTAMP "\nPlease remove if found\n";

close DATESTAMP or die "Could not close datestamp file: $!\n";

ok(1);

# Now get the version
my ($maj, $min, $patch) = starversion('this_prog');


ok($maj, 5);
ok($min, 16);
ok($patch, 4);
