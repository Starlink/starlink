#!/usr/local/bin/perl -w

# This is a simple test script. It creates a noticeboard, ties it to
# a hash and sets some values. These values are then retrieved and
# compared with the original value.
# The structure of the notice board is (from Data::Dumper):

#VAR1 = {
#          'CURRENT_CONFIG' => 'magnificent',
#          'CURRENT_STATUS' => [
#                                '52.5',
#                                '76.2'
#                              ],
#          'FIBRE_PARAMETERS' => {
#                                  'TRANS_MATRIX' => [
#                                                      1,
#                                                      2,
#                                                      3,
#                                                      4
#                                                    ]
#                                }
#        };


use strict;
use Starlink::NBS qw/:nbslib/;

use vars qw/$loaded $NUM/;
BEGIN { $| = 1; print "1..10\n"; }
END {print "not ok 1\n" unless $loaded;}
$loaded = 1;
$NUM = 1;
&ok;

my ($status, $sid, $fibsid, $topsid, @dims);

my $good = $status = &Starlink::NBS::SAI__OK;

# Create a new noticeboard

nbs_begin_definition($topsid, $status);
check_status($status);


nbs_define_primitive($topsid, 'CURRENT_CONFIG','_CHAR', 0,132, $sid, $status);
nbs_define_primitive($topsid, 'CURRENT_STATUS', '_DOUBLE', 1,16, $sid, $status);
check_status($status);

@dims = ( 72 );
nbs_define_shape($sid, 1, @dims, $status);
nbs_define_structure($topsid, 'FIBRE_PARAMETERS','FIBRE_PARAMETERS', $fibsid, $status);
check_status($status);

nbs_define_primitive($fibsid, 'TRANS_MATRIX','_INTEGER',2, 4*4, $sid, $status);
check_status($status);

@dims = (2,2);
nbs_define_shape($sid, 2, @dims, $status);

#nbs_end_definition('AUTOFIB','DEFINITION_SAVE', $status);

#nbs_restore_definition('AUTOFIB', 'AUTOFIB', $status);

nbs_end_definition('AUTOFIB','CREATE_NOTICEBOARD', $status);
check_status($status);

my (%top, $nbs);

$nbs = new Starlink::NBS("AUTOFIB");
defined $nbs ? &ok : &notok;
die unless defined $nbs;

tie(%top, ref($nbs), $nbs);

$top{CURRENT_CONFIG} = 'magnificent';

# Check the return value
if ($top{CURRENT_CONFIG} eq 'magnificent') {
  &ok;
} else {
  &notok;
}
print "WHHOPS\n";

$top{CURRENT_STATUS} = [52.5,76.2]; 

if ($top{CURRENT_STATUS}->[0] == 52.5) {
  &ok;
} else {
  &notok;
}


$top{FIBRE_PARAMETERS}{TRANS_MATRIX} = [1,2,3,4];

if ($top{FIBRE_PARAMETERS}{TRANS_MATRIX}->[2] == 3) {
  &ok;
} else {
  &notok;
}

# Sub to check status
sub check_status {
  my $status = shift;
  if ($status == $good) {
    &ok; 
  } else {
    &notok;
  }
}

sub ok {
  print "ok $NUM\n";
  $NUM++;
}

sub notok {
  print "not ok $NUM\n";
  $NUM++;
}
