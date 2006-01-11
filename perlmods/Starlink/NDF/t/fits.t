#!perl -w

use Test::More tests => 10;
use strict;
no strict "vars";

use_ok("NDF");

# ================================================================
#   Test NDF calls to read FITS extension information
#   from test.sdf
# ================================================================

# Test file
my $file = "test";

# initialise global status
my $status = &NDF::SAI__OK;

# Initialise NDF
err_begin($status);
ndf_begin();

# Open up the test file
die "Couldn't find test file: $file\n" unless (-e "$file.sdf");

ndf_find(&NDF::DAT__ROOT, $file, my $indf, $status);
is($status, &NDF::SAI__OK, "Check status");

my $wcs = ndfGtwcs( $indf, $status );
is($status, &NDF::SAI__OK, "Check ndfGtwcs status");

ndf_xloc($indf, 'FITS', 'READ', my $floc, $status);
is($status, &NDF::SAI__OK, "Check status");

my @fits;
dat_get1c($floc, 200, \@fits, my $nfits, $status);

is($nfits, 129, "Count FITS array");
is($status, &NDF::SAI__OK, "Check status");

# Test the fits calls
$inst = fits_get_item(\@fits, 'INSTRUME');

is($inst, "SCUBA", "Check instrument");

undef $comment;
($name, $value, $comment) = fits_get_nth_item(\@fits, 3);

is($name, "ALT-OBS", "Check FITS name");
is($value, 4092, "Check value");

# Clean up and close the file

ndf_annul($indf, $status);
ndf_end($status);
is($status, &NDF::SAI__OK, "Check status");
err_end($status);





