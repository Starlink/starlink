#!perl -w

use Test::More tests => 16;
use strict;

use_ok( "NDF" );

# ================================================================
#   Test NDF calls to read HISTORY information
#   from test.sdf
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
ndf_happn("Perl test", $status);

# Copy the reference file for testing purposes
ndf_place( &NDF::DAT__ROOT, $file, my $place, $status );
is( $status, &NDF::SAI__OK, "check status");
ndf_find( &NDF::DAT__ROOT, $oldfile, my $oldndf, $status );
ndf_copy($oldndf, $place, my $indf, $status );
is( $status, &NDF::SAI__OK, "check status");

ndf_hcre($indf, $status);
is( $status, &NDF::SAI__OK, "check status");
ndf_hnrec($indf, my $nnrec, $status);
is( $status, &NDF::SAI__OK, "check status");
print "# Number of records: $nnrec\n";

my @date = (1996, 8, 8, 20, 00);
ndf_hfind($indf, \@date, 0.0, 0, my $irec, $status);
is( $status, &NDF::SAI__OK, "check status");
print "# Record is $irec\n";
ndf_hdef($indf,' ',$status);
is( $status, &NDF::SAI__OK, "check status");

my @text = ("This is a test of history. (Last word should be HI)", 
	    "Text should be formatted.", "HI");
ndf_hput('NORMAL', '',0, 3, \@text, 1, 1, 1, $indf, $status);
is( $status, &NDF::SAI__OK, "check status");
ndf_hend($status);
is( $status, &NDF::SAI__OK, "check status");

ndf_hnrec($indf, my $nrec, $status);
is( $status, &NDF::SAI__OK, "check status");
ndf_hout($indf, $nrec, $status);
is( $status, &NDF::SAI__OK, "check status");
ndf_hinfo($indf, 'APPLICATION', $nrec, my $val, $status);
is( $status, &NDF::SAI__OK, "check status");

ndf_delet($indf, $status);
is( $status, &NDF::SAI__OK, "check status");
ok( !-e "$file.sdf", "File no longer exists");

ndf_end($status);
is( $status, &NDF::SAI__OK, "check status");
err_end($status);

is(($nrec-$nnrec),1,"Test hist diff");
