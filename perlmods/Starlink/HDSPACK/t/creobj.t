#!perl -w

# Test of creobj function

use strict;
use Test;
BEGIN { plan tests => 8 }
use Starlink::HDSPACK qw/ creobj /;

$Starlink::HDSPACK::DEBUG = 1;

# Get good status
my $status = &Starlink::HDSPACK::SAI__OK;
my $good = $status;



# Now need to set up a target
my $out = "out$$";
my @dims = (32,48);

# First the file
$status = creobj($out, 'NDF',$status);
ok($status, $good);

#DAT_NEW( NLOC, 'DATA_ARRAY', '_UBYTE', 2, DIMS, STATUS )
$status = creobj($out .".DATA_ARRAY", '_UBYTE', \@dims,$status);
ok($status, $good);

#DAT_NEWC( NLOC, 'LABEL', 80, 0, 0, STATUS )
$status = creobj($out .".LABEL", '_CHAR*80', $status);
ok($status, $good);

#DAT_NEW( NLOC, 'AXIS', 'AXIS', 1, 2, STATUS )
$status = creobj($out. ".AXIS", 'AXIS', [2], $status);
ok($status, $good);

# Create AXIS(1).DATA_ARRAY
$status = creobj($out. ".AXIS(1).DATA_ARRAY", '_REAL', [$dims[0]], $status);
ok($status, $good);

# Crate AXIS(1).LABEL
$status = creobj($out. ".AXIS(1).LABEL", '_CHAR*30', $status);
ok($status, $good);

# Create AXIS(2).DATA_ARRAY
$status = creobj($out. ".AXIS(2).DATA_ARRAY", '_REAL', [$dims[1]], $status);
ok($status, $good);

# Crate AXIS(2).LABEL
$status = creobj($out. ".AXIS(2).LABEL", '_CHAR*30', $status);
ok($status, $good);



unlink "${out}.sdf"
  or die "Error removing temporary file";

exit;

