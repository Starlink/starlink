#!/usr/local/bin/perl -w

# Test miscellaneous EMS functions

use strict;
use Test;
BEGIN { plan tests => 4 }
use Starlink::EMS;

my $ok = SAI__OK;
my $status = $ok;

# Start ems
ems_begin( $status );
ok($status, $ok);

# Tuning

# SZOUT - word wrapping
my $string = "Specifies a maximum line length to be used in the line wrapping process.";

# Need to redirect STDOUT to another file so that we can test it
# No time for this at the moment.

# STREAM - tests control codes and turns off line wrapping

# Need to redirect STDOUT to another file so that we can test it

# Test emsStat

# Should be SAI__OK first
emsMark();
ok(emsStat, SAI__OK);

$status = SAI__WARN;
emsRep("", "A bogus warning", $status);
ok(emsStat(), $status);
emsAnnul($status);
emsRlse();

# End ems
ems_end( $status );

ok($status, $ok);



