#!/usr/local/bin/perl -w

use strict;
use Test;
BEGIN { plan tests => 9 }
use Starlink::EMS qw/:ems :sai /;

my $ok = SAI__OK;
my $status = $ok;

# Start ems
ems_begin( $status );
ok($status, $ok);

# Start a new reporting level
ems_mark;
ok(ems_level(), 3);
ems_rlse;

$status = SAI__ERROR;

ems_rep("PARAM", "This is meant to be an error", $status);

$status = SAI__WARN;
ems_rep("PAR2", "This is meant to be a warning", $status);

# Retrieve the above error messages
my ($par, $str, $lstat) = ems_eload();

ok( $par, 'PARAM');
ok( $lstat, SAI__ERROR);
ok( $str, 'This is meant to be an error');

($par, $str, $lstat) = ems_eload();

ok( $par, 'PAR2');
ok( $lstat, SAI__WARN);
ok( $str, 'This is meant to be a warning');


# Tidy up status
ems_annul( $status );

# End ems
ems_end( $status );
ok( $status, $ok);

