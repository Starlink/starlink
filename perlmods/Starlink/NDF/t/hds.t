#!perl -w

use Test::More tests => 15;

use warnings;
use strict;

use_ok( "NDF" );

# ================================================================
#   Test HDS calls
#   This is hds_test.f converted to perl
# ================================================================

# initialise global status
my $status = &NDF::SAI__OK;

# Initialise the dimension array
my @dim = (10,20);

# Create a new container file 

hds_new('hds_test', 'HDS_TEST', 'NDF', 0, \@dim, my $loc, $status);
is( $status, &NDF::SAI__OK, "check status");

dat_new($loc, 'DATA_ARRAY', '_INTEGER', 2, \@dim, $status);
is( $status, &NDF::SAI__OK, "check status");

# Find and map the data array
dat_find($loc, 'DATA_ARRAY', my $nloc, $status);
is( $status, &NDF::SAI__OK, "check status");
dat_mapv($nloc, '_REAL', 'WRITE', my $pntr, my $el, $status);
is( $status, &NDF::SAI__OK, "check status");

# Make an array
my @data=();
foreach (1..$el) {
  push(@data,$_);
}

if ($status == &NDF::SAI__OK) {
  array2mem(\@data, "f*", $pntr);
}

# Clean up and close the file
dat_unmap($nloc, $status);
is( $status, &NDF::SAI__OK, "check status");
dat_annul($nloc, $status);
is( $status, &NDF::SAI__OK, "check status");
dat_annul($loc, $status);
is( $status, &NDF::SAI__OK, "check status");

# Re-open the file

hds_open('hds_test', 'UPDATE', $loc, $status);
is( $status, &NDF::SAI__OK, "check status");

# Find and map the data array
dat_find($loc, 'DATA_ARRAY', $nloc, $status);
is( $status, &NDF::SAI__OK, "check status");
dat_mapv($nloc, '_INTEGER', 'READ', $pntr, $el, $status);
is( $status, &NDF::SAI__OK, "check status");

# Sum the elements
if ($status == &NDF::SAI__OK) {
  @data = mem2array($pntr, "i*", $el);
}

my $sum = 0;
for ( @data) { $sum += $_; }
is( $sum, 20100, "Check sum");

# Clean up and close the file
dat_unmap($nloc, $status);
is( $status, &NDF::SAI__OK, "check status");
dat_annul($nloc, $status);
is( $status, &NDF::SAI__OK, "check status");
hds_erase($loc, $status);
is( $status, &NDF::SAI__OK, "check status");

