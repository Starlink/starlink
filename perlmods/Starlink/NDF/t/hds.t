#!perl -w

use strict;
no strict "vars";

use NDF;

# ================================================================
#   Test HDS calls
#   This is hds_test.f converted to perl
# ================================================================

$n=1; # number of tests
print "1..$n\n";

# initialise global status
$status = &NDF::SAI__OK;

# Initialise the dimension array
@dim = (10,20);

# Create a new container file 

hds_new('hds_test', 'HDS_TEST', 'NDF', 0, @dim,  $loc, $status);
dat_new($loc, 'DATA_ARRAY', '_INTEGER', 2, @dim, $status);

# Find and map the data array
dat_find($loc, 'DATA_ARRAY', $nloc, $status);
dat_mapv($nloc, '_REAL', 'WRITE', $pntr, $el, $status);

# Make an array
@data=();
foreach (1..$el) {
  push(@data,$_);
}

if ($status == &NDF::SAI__OK) {
  array2mem(@data, "f*", $pntr);
}

# Clean up and close the file
dat_unmap($nloc, $status);
dat_annul($nloc, $status);
dat_annul($loc, $status);

# Re-open the file

hds_open('hds_test', 'UPDATE', $loc, $status);

# Find and map the data array
dat_find($loc, 'DATA_ARRAY', $nloc, $status);
dat_mapv($nloc, '_INTEGER', 'READ', $pntr, $el, $status);

# Sum the elements
if ($status == &NDF::SAI__OK) {
  @data = mem2array($pntr, "i*", $el);
}

$sum = 0;
grep($sum += $_, @data);

# Clean up and close the file
dat_unmap($nloc, $status);
dat_annul($nloc, $status);
hds_erase($loc, $status);

(($status == &NDF::SAI__OK) && ($sum == 20100)) && 
  (print "ok\n") || (print "not ok\n");

