#!/usr/local/bin/perl -w

BEGIN { $| = 1; print "1..5\n"; }
END {print "not ok 1\n" unless $loaded;}
use Starlink::EMS qw/:ems :sai /;
$loaded = 1;
print "ok 1\n";

$count = 1;
$ok = SAI__OK;
$status = $ok;

# Start ems
ems_begin( $status );
&test( $status == $ok);

# Copy some values into some tokens and retrieve them
ems_setl('LOG', 1);
$mstr = ems_mload("","^LOG", $status);

&test( $mstr eq 'TRUE' );

ems_fmtr('FLT', 'F5.2', 3.141592654);
$mstr = ems_mload("",'^FLT', $status);

&test( $mstr eq ' 3.14' );

# End ems
ems_end( $status );

&test( $status == $ok);


sub test {
  $count++;
  if ($_[0]) {
    print "ok $count\n";
  } else {
    print "not ok $count\n";
  }
}
