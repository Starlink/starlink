#!perl -w

use strict;
no strict "vars";

use NDF;

# ================================================================
#   Test NDF calls to read FITS extension information
#   from test.sdf
# ================================================================

# Test file
$file = "test";

$n=4; # number of tests
print "1..$n\n";

# initialise global status
$status = &NDF::SAI__OK;

# Initialise NDF
ndf_begin;

# Open up the test file
die "Couldn't find test file: $file\n" unless (-e "$file.sdf");

ndf_find(&NDF::DAT__ROOT, $file, $indf, $status);


ndf_xloc($indf, 'FITS', 'READ', $floc, $status);
dat_get1c($floc, 200, @fits, $nfits, $status);

(($status == &NDF::SAI__OK) && ($nfits == 129)) && (print "ok\n") || 
  (print "not ok\n");

# Test the fits calls
$inst = fits_get_item(@fits, 'INSTRUME');

($inst eq 'SCUBA') && (print "ok\n") || (print "not ok\n");

undef $comment;
($name, $value, $comment) = fits_get_nth_item(@fits, 3);

(($name eq "ALT-OBS") && ($value == 4092)) && (print "ok\n") || 
  (print "not ok\n");

# Clean up and close the file

ndf_annul($indf, $status);
ndf_end($status);

($status != &NDF::SAI__OK) && do { print "not ok\n"; exit;} || (print "ok\n");





