package Starlink::EMS;

use strict;
use Carp;
use vars qw($VERSION @ISA @EXPORT %EXPORT_TAGS);

require Exporter;
require DynaLoader;

@ISA = qw(Exporter DynaLoader);

# Export the main routine
@EXPORT = qw(
	     ems1_get_facility_error
);

%EXPORT_TAGS = (
		'ems' => [qw/
			  ems_annul ems_begin ems_eload ems_end
			  ems_errno ems_facer ems_expnd
			  ems_level ems_mark ems_mload
			  ems_renew ems_rep ems_rlse
			  ems_setc ems_setd ems_seti ems_setl ems_setr
			  ems_stat ems_syser ems_tune
			  get_facility_error
			  /],
		'Ems' => [qw/
			  emsAnnul emsBegin emsEload emsEnd emsExpnd
			  emsErrno emsFacer emsFmtc emsFmtd emsFmti
			  emsFmtl emsFmtr emsLevel emsMark emsMload
			  emsRenew emsRep emsRlse emsSetc emsSetd
			  emsSeti emsSetl emsSetr emsStat emsSyser
			  emsTune
			  /],
		'emserr' => [qw/
			     EMS__NOMSG
			     /],
		'sai'    => [qw/
			     SAI__ERROR SAI__OK SAI__WARN
			     /]
	       );

Exporter::export_tags('ems','emserr','sai','Ems');

$VERSION = '2.0';

bootstrap Starlink::EMS $VERSION;

sub get_facility_error ($) {
  my ($fac, $id, $text);
  ems1_get_facility_error($_[0], $fac, $id, $text);
  return ($fac, $id, $text);
}


1;
__END__


=head1 NAME

Starlink::EMS - Perl extension for Starlink EMS library

=head1 SYNOPSIS

  use Starlink::EMS;
  use Starlink::EMS qw/:ems :sai/;

  $ok = SAI__OK;

  emsBegin($status = $ok);

  $status = SAI__ERROR;

  emsSetc('TOKEN', $string);
  emsRep($par, $error, $status);

  ($par, $str, $status) = emsEload;
  $level = emsLevel;

  emsEnd( $status );


  ems1_get_facility_error($status, $facility, $ident, $text);


=head1 DESCRIPTION

This module provides a simple interface to the Starlink EMS library.
All C routines are available plus an extra internal routine
for converting a status code directly to the error mesage
components (ems1_get_facility_error).

The ems function can be imported using the 'ems' tag:

 use Starlink::EMS qw( :ems );

for the Fortran-style names or 'Ems' for the C-style names:

 use Starlink::EMS qw/ :Ems /;

The following routines are available:

emsAnnul(), emsBegin(), emsEload(), emsEnd(), emsErrno(), emsExpnd(),
emsFacer(), emsLevel(), emsMark(), emsRenew(), emsRep(),
emsRlse(), emsSetc(), emsSetd(), emsSeti(), emsSetl(), emsSetr(),
emsStat(), emsSyser(), emsTune().

The C-style names are preferred in new code.

These routines are explained in SSN/4. One difference between the
perl implementation and the C/Fortran version described in SSN/4
concerned return values. Routines with arguments that contain pure return
values (as opposed to modifications of an existing variable) return the
values as a perl list. The routines are:

  ($par, $str, $status) = emsLoad();
  $level = emsLevel();
  $string = emsExpnd( $str, $status );

Use the ERR_ calls (available from the C<NDF> perl module) for a
higher level interface to the error message system.

=head1 EXTRA ROUTINES

The ems1_get_facility_error() routine is not documented in SSN/4
but can be used to convert an error status to the corresponding error code
and text.

This is similar to the ems_facer() routine (also available) but gives more
control over the use of this information and does not go via a token.

This command is available in two forms. One form is identical to the C
implementation, the other provides the values as a perl array:

  ems1_get_facility_error($status, $facility, $ident, $text);
  ($facility, $ident, $text) = get_facility_error($status);

=head1 DEPRECATED ROUTINES

As of version 2.0 of EMS the C<emsMload> function has been
deprecated. It is still available in this module although
new code should use C<emsExpnd> rather than C<emsMload>.

  $string = emsMload( "", $str, $status );
  $string = emsExpnd( $str, $status );

=head1 CONSTANTS

The following constants are available: SAI__OK, SAI__ERROR,
SAI__WARN. They are imported via the 'sai' tag:

  use Starlink::EMS qw( :sai );

=head1 SEE ALSO

Starlink System Note 4 (SSN/4).
L<NDF>

=head1 REVISION

$Id$

=head1 AUTHOR

Tim Jenness E<lt>t.jenness@jach.hawaii.eduE<gt>

=head1 COPYRIGHT

Copyright (C) Particle Physics and Astronomy Research Council 1998-2000.
All Rights Reserved.

=cut
