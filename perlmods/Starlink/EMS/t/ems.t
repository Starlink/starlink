#!/usr/local/bin/perl -w

BEGIN { $| = 1; print "1..10\n"; }
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

# Start a new reporting level
ems_mark;

&test( ems_level() == 3);

ems_rlse;

$status = SAI__ERROR;

ems_rep("PARAM", "This is meant to be an error", $status);

$status = SAI__WARN;
ems_rep("PAR2", "This is meant to be a warning", $status);

# Retrieve the above error messages
($par, $str, $lstat) = ems_eload();

&test( $par eq 'PARAM');
&test( $lstat == SAI__ERROR);
&test( $str eq 'This is meant to be an error');

($par, $str, $lstat) = ems_eload();

&test( $par eq 'PAR2');
&test( $lstat == SAI__WARN);
&test( $str eq 'This is meant to be a warning');


# Tidy up status
ems_annul( $status );

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
