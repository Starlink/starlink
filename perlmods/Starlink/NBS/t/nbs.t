#!/bin/perl -w

use strict;

use blib;

use Starlink::NBS qw/:nbslib/;

my ($status, $sid, $fibsid, $topsid, @dims);

# Create a new noticeboard

$status = &Starlink::NBS::SAI__OK;

nbs_begin_definition($topsid, $status);

nbs_define_primitive($topsid, 'CURRENT_CONFIG','_CHAR', 0,132, $sid, $status);

nbs_define_primitive($topsid, 'CURRENT_STATUS', 'CURRENT_STATUS', 1, 72*64, $sid, $status);

@dims = ( 72 );
nbs_define_shape($sid, 1, @dims, $status);
nbs_define_structure($topsid, 'FIBRE_PARAMETERS','FIBRE_PARAMETERS', $fibsid, $status);

nbs_define_primitive($fibsid, 'TRANS_MATRIX','_INTEGER',2, 4*4, $sid, $status);

@dims = (2,2);
nbs_define_shape($sid, 2, @dims, $status);

#nbs_end_definition('AUTOFIB','DEFINITION_SAVE', $status);

#nbs_restore_definition('AUTOFIB', 'AUTOFIB', $status);

nbs_end_definition('AUTOFIB','CREATE_NOTICEBOARD', $status);

my (%top, $nbs);

$nbs = new Starlink::NBS("AUTOFIB");

tie(%top, ref($nbs), $nbs);

$top{CURRENT_CONFIG} = 'gfhe';


foreach (keys %top) {
  if (defined $top{$_}) {
    print "Key: $_: $top{$_}\n";
  } else {
    print "Key: $_\n";
  }

}
