#!perl

use strict;
use Test;
BEGIN { plan tests => 3 }
use Starlink::EMS;

# Test the status handling by converting SAI__ERROR to
# a code and back again

my $status = Starlink::EMS::SAI__ERROR;

# Translate error to a string
ems1_get_facility_error($status, my $fac, my $ident, my $text);
my $err = $fac . "__$ident";

ok($err, "SAI__ERROR");

# Try different format
($fac, $ident, $text) = get_facility_error( Starlink::EMS::SAI__WARN );
ok($fac, 'SAI');
ok($ident,'WARN');
