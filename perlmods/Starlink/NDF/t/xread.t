#!perl -w

use strict;
no strict "vars";

use NDF;

# ================================================================
#   Test NDF calls to read extension information
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

# How many extensions
ndf_xnumb($indf, $num, $status);

(($status == &NDF::SAI__OK) && ($num == 3)) && (print "ok\n") || 
  (print "not ok\n");


# Read in a value
$tcold = -1;
ndf_xgt0r($indf, 'REDS','T_COLD',$tcold, $status);

(($status == &NDF::SAI__OK) && ($tcold == 55.0)) && (print "ok\n") || 
  (print "not ok\n");



$sub = 'n/a';
ndf_xgt0c($indf, 'REDS','SUB_INSTRUMENT',$sub,$status);

(($status == &NDF::SAI__OK) && ($sub eq 'LONG')) && (print "ok\n") || 
  (print "not ok\n");


# Clean up and close the file

ndf_annul($indf, $status);
ndf_end($status);

($status != &NDF::SAI__OK) && do { print "not ok\n"; exit;} || (print "ok\n");





