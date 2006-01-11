#!perl -w

use Test::More tests => 24;
use warnings;
use strict;

use_ok( "NDF" );

# ================================================================
#   Test NDF calls to write extension information
#   test.sdf
# ================================================================

# Test file
my $oldfile = "test";
my $file = "twrite";

# initialise global status
my $status = &NDF::SAI__OK;

# Initialise NDF
err_begin($status);
ndf_begin();

# Open up the test file
die "Couldn't find test file: $oldfile\n" unless (-e "$oldfile.sdf");

# Copy the reference file for testing purposes
ndf_place( &NDF::DAT__ROOT, $file, my $place, $status );
is( $status, &NDF::SAI__OK, "check status");
ndf_find( &NDF::DAT__ROOT, $oldfile, my $oldndf, $status );
ndf_copy($oldndf, $place, my $indf, $status );
is( $status, &NDF::SAI__OK, "check status");

# Add an extension
my @dim = ();
ndf_xnew($indf, 'TEST', 'PERL_TEST', 0, \@dim, my $loc, $status);
is($status, &NDF::SAI__OK, "Check status");

@dim = (1,2);
ndf_xnew($indf, 'ARY_TEST', 'PERL_TEST_ARR', 2, \@dim, $loc, $status);
is($status, &NDF::SAI__OK, "Check status");

# Add some data

my $cinval = "hello";
my $dinval = 3.141592654456;
my $iinval = 5;
my $linval = 1;
my $rinval = 26.8;

ndf_xpt0c($cinval, $indf, 'TEST', 'CHAR', $status);
is($status, &NDF::SAI__OK, "Check status");
ndf_xpt0d($dinval, $indf, 'TEST', 'DBL', $status);
is($status, &NDF::SAI__OK, "Check status");
ndf_xpt0i($iinval, $indf, 'TEST', 'INT', $status);
is($status, &NDF::SAI__OK, "Check status");
ndf_xpt0l($linval, $indf, 'TEST', 'LOG', $status);
is($status, &NDF::SAI__OK, "Check status");
ndf_xpt0r($rinval, $indf, 'TEST', 'REAL', $status);

# Read it back
my $cval = '';
my $dval = 0.0;
my $ival = 0;
my $lval = 0;
my $rval = 0.0;
ndf_xgt0c($indf, 'TEST', 'CHAR', $cval, $status);
is($status, &NDF::SAI__OK, "Check status");
ndf_xgt0d($indf, 'TEST', 'DBL', $dval, $status);
is($status, &NDF::SAI__OK, "Check status");
ndf_xgt0i($indf, 'TEST', 'INT', $ival, $status);
is($status, &NDF::SAI__OK, "Check status");
ndf_xgt0l($indf, 'TEST', 'LOG', $lval, $status);
is($status, &NDF::SAI__OK, "Check status");
ndf_xgt0r($indf, 'TEST', 'REAL', $rval, $status);
is($status, &NDF::SAI__OK, "Check status");

# delete the extensions
ndf_xdel($indf, 'TEST', $status);
is($status, &NDF::SAI__OK, "Check status");
ndf_xdel($indf, 'ARY_TEST', $status);
is($status, &NDF::SAI__OK, "Check status");

ndf_delet($indf, $status);
is( $status, &NDF::SAI__OK, "check status");
ok( !-e "$file.sdf", "File no longer exists");

ndf_end($status);
is($status, &NDF::SAI__OK, "Check status");
err_end($status);

is( $cval, $cinval, "Compare CHAR");
is( $lval, $linval, "Compare LOGICAL");
is( $ival, $iinval, "Compare INTEGER");
is( $dval, $dinval, "Compare DOUBLE");

# deal with rounding
is( sprintf("%.1f", $rval), $rinval, "Compare REAL");


