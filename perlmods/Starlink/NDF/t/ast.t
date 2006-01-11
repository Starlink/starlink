#!perl -w

use Test::More;
use strict;

# Skip if we have no Starlink::AST
eval {
  require Starlink::AST;
};
if ($@) {
  plan skip_all => 'Starlink::AST not installed';
} else {
  plan tests => 4;
}

use_ok("NDF");

# ================================================================
#   Test NDF calls to read WCS
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

# Clean up and close the file

ndf_annul($indf, $status);
ndf_end($status);
is($status, &NDF::SAI__OK, "Check status");
err_end($status);





