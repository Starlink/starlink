#!perl -w

# Test of wrapper functions

use strict;
use Test;
BEGIN { plan tests => 10 }
use Starlink::HDSPACK qw/ create_hdsobj delete_hdsobj copy_hdsobj/;

$Starlink::HDSPACK::DEBUG = 1;

# Now need to set up a target
my $out = "out$$";
my @dims = (32,48);
my $status;

# First the file
$status = create_hdsobj($out, 'NDF');
ok($status);

#DAT_NEW( NLOC, 'DATA_ARRAY', '_UBYTE', 2, DIMS, STATUS )
$status = create_hdsobj($out .".DATA_ARRAY", '_UBYTE', \@dims);
ok($status);

#DAT_NEWC( NLOC, 'LABEL', 80, 0, 0, STATUS )
$status = create_hdsobj($out .".LABEL", '_CHAR*80');
ok($status);

#DAT_NEW( NLOC, 'AXIS', 'AXIS', 1, 2, STATUS )
$status = create_hdsobj($out. ".AXIS", 'AXIS', [2]);
ok($status);

# Create AXIS(1).DATA_ARRAY
$status = create_hdsobj($out. ".AXIS(1).DATA_ARRAY", '_REAL', [$dims[0]]);
ok($status);

# Crate AXIS(1).LABEL
$status = create_hdsobj($out. ".AXIS(1).LABEL", '_CHAR*30');
ok($status);

# Create AXIS(2).DATA_ARRAY
$status = create_hdsobj($out. ".AXIS(2).DATA_ARRAY", '_REAL', [$dims[1]]);
ok($status);

# Crate AXIS(2).LABEL
$status = create_hdsobj($out. ".AXIS(2).LABEL", '_CHAR*30');
ok($status);

# Delete the label
ok(delete_hdsobj($out.".AXIS(2).LABEL"));

# Copy something
ok(copy_hdsobj($out.".LABEL", $out.".AXIS(2).LABEL2"));

unlink "${out}.sdf"
  or die "Error removing temporary file";

exit;

