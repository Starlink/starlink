#!perl -w

use strict;
no strict "vars";

use NDF;

# ================================================================
#   Test NDF calls
#    - NDF version of hds.t
# ================================================================

$n=1; # number of tests
print "1..$n\n";

# initialise global status
$status = &NDF::SAI__OK;

# Initialise the dimension array
@ubnd = (10,20);
@lbnd = (1,1);


# Initialise NDF
ndf_begin;

# Create a new container file 
ndf_place(&NDF::DAT__ROOT, 'ndf_test', $place, $status);
ndf_new('_INTEGER', 2, @lbnd, @ubnd, $place, $indf, $status);


# Map the data array
ndf_map($indf, 'DATA', '_INTEGER', 'WRITE', $pntr, $el, $status);


# Make an array
@data=();
foreach (1..$el) {
  push(@data,$_);
}

array2mem(@data, "i*", $pntr) if ($status == &NDF::SAI__OK);

# Clean up and close the file
ndf_unmap($indf, 'DATA',  $status);
ndf_annul($indf, $status);


($status != &NDF::SAI__OK) && do { print "not ok\n"; exit;};

# Re-open the file

ndf_find(&NDF::DAT__ROOT,'ndf_test',$indf, $status);

# Check the dimensions
$maxdims = 100;
ndf_dim($indf, $maxdims, @dim, $ndim, $status);

print "Dims are: ", join(" ",@dim), " [$ndim dimensions]\n";

# Find and map the data array
ndf_map($indf, 'DATA', '_INTEGER', 'READ', $pntr, $el, $status);


# Sum the elements
@data = mem2array($pntr, "i*", $el) if ($status == &NDF::SAI__OK);

$sum = 0;
grep($sum += $_, @data);


# Clean up and close the file
ndf_unmap($indf, 'DATA', $status);
ndf_annul($indf, $status);
ndf_end($status);

unlink("ndf_test.sdf");

(($status == &NDF::SAI__OK) && ($sum == 20100)) && 
  (print "ok\n") || (print "not ok\n");


