#!perl -w

use strict;
no strict "vars";

use NDF;

# ================================================================
#   Test NDF calls to write extension information
#   test.sdf
# ================================================================

# Test file
$oldfile = "test";
$file = "twrite";

system("cp $oldfile.sdf $file.sdf");


$n=1; # number of tests
print "1..$n\n";

# initialise global status
$status = &NDF::SAI__OK;

# For -w
$place = 0;


# Initialise NDF
ndf_begin;

# Open up the test file
die "Couldn't find test file: $file\n" unless (-e "$file.sdf");

ndf_open(&NDF::DAT__ROOT,$file,'UPDATE','OLD',$indf,$place,$status);

# Add an extension
@dim = ();
ndf_xnew($indf, 'TEST', 'PERL_TEST', 0, @dim, $loc, $status);

@dim = (1,2);
ndf_xnew($indf, 'ARY_TEST', 'PERL_TEST_ARR', 2, @dim, $loc, $status);

# Add some data

$cinval = "hello";
$dinval = 3.141592654456;
$iinval = 5;
$linval = 1;
$rinval = 26.8;

ndf_xpt0c($cinval, $indf, 'TEST', 'CHAR', $status);
ndf_xpt0d($dinval, $indf, 'TEST', 'DBL', $status);
ndf_xpt0i($iinval, $indf, 'TEST', 'INT', $status);
ndf_xpt0l($linval, $indf, 'TEST', 'LOG', $status);
ndf_xpt0r($rinval, $indf, 'TEST', 'REAL', $status);

# Read it back
$cval = $dval = $ival = $lval = $rval = 0;
ndf_xgt0c($indf, 'TEST', 'CHAR', $cval, $status);
ndf_xgt0d($indf, 'TEST', 'DBL', $dval, $status);
ndf_xgt0i($indf, 'TEST', 'INT', $ival, $status);
ndf_xgt0l($indf, 'TEST', 'LOG', $lval, $status);
ndf_xgt0r($indf, 'TEST', 'REAL', $rval, $status);

# delete the extensions
ndf_xdel($indf, 'TEST', $status);
ndf_xdel($indf, 'ARY_TEST', $status);

ndf_annul($indf, $status);
ndf_end($status);

unlink("$file.sdf") || die "EEK! Couldnt remove file";

print "CHAR: $cval (should be $cinval)\n";
print "LOGICAL: $lval (should be $linval)\n";
print "INT:  $ival (should be $iinval)\n";
print "DOUBLE: $dval (should be $dinval)\n";
print "REAL:  $rval (should be $rinval but will be slightly out)\n";

$rval = sprintf("%4.1f", $rval); # Deal with rounding errors
                                 # Between real and double

if (($status == &NDF::SAI__OK) &
    ($cval eq $cinval) &
    ($dval == $dinval) &
    ($ival == $iinval) &
    ($lval == $linval) &
    ($rval == $rinval) ) {
  print "ok\n";
} else {
  print "not ok\n";
}

