#!perl -w

use Test::More tests => 12;
use strict;

use_ok("NDF");

# ================================================================
#   Test NDF calls
#    - NDF version of hds.t
# ================================================================

# initialise global status
my $status = &NDF::SAI__OK;

# Initialise the dimension array
my @ubnd = (10,20);
my @lbnd = (1,1);


# Initialise NDF
ndf_begin();

# Create a new container file 
ndf_place(&NDF::DAT__ROOT, 'ndf_test', my $place, $status);
is( $status, &NDF::SAI__OK, "check status ndf_place");

ndf_new('_INTEGER', 2, \@lbnd, \@ubnd, $place, my $indf, $status);
is( $status, &NDF::SAI__OK, "check status ndf_new");

# Map the data array
ndf_map($indf, 'DATA', '_INTEGER', 'WRITE', my $pntr, my $el, $status);
is( $status, &NDF::SAI__OK, "check status ndf_map");

# Make an array
my @data=();
foreach (1..$el) {
  push(@data,$_);
}

array2mem(\@data, "i*", $pntr) if ($status == &NDF::SAI__OK);

# Clean up and close the file
ndf_unmap($indf, 'DATA',  $status);
is( $status, &NDF::SAI__OK, "check status ndf_unmap");
ndf_annul($indf, $status);
is( $status, &NDF::SAI__OK, "check status ndf_annul");

# Re-open the file

ndf_find(&NDF::DAT__ROOT,'ndf_test',$indf, $status);
is( $status, &NDF::SAI__OK, "check status ndf_find");

# Check the dimensions
my $maxdims = 100;
my @dim;
my $ndim;
ndf_dim($indf, $maxdims, \@dim, $ndim, $status);
is( $status, &NDF::SAI__OK, "check status ndf_dim");

print "# Dims are: ", join(" ",@dim), " [$ndim dimensions]\n";

# Find and map the data array
ndf_map($indf, 'DATA', '_INTEGER', 'READ', $pntr, $el, $status);
is( $status, &NDF::SAI__OK, "check status ndf_map");

# Sum the elements
@data = mem2array($pntr, "i*", $el) if ($status == &NDF::SAI__OK);

my $sum = 0;
for ( @data) { $sum += $_; }
is( $sum, 20100, "Check sum");

# Clean up and close the file
ndf_unmap($indf, 'DATA', $status);
is( $status, &NDF::SAI__OK, "check status ndf_unmap");
ndf_annul($indf, $status);
ndf_end($status);

unlink("ndf_test.sdf");

is( $status, &NDF::SAI__OK, "check status");
