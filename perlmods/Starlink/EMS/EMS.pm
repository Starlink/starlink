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
	     SAI__OK SAI__ERROR SAI__WARN
);

%EXPORT_TAGS = (
		'ems' => [qw/
			  ems_annul ems_begin ems_eload ems_end
			  ems_errno ems_facer
			  ems_fmtc ems_fmtd ems_fmti ems_fmtl ems_fmtr
			  ems_level ems_mark ems_mload
			  ems_renew ems_rep ems_rlse
			  ems_setc ems_setd ems_seti ems_setl ems_setr
			  ems_stat ems_syser
			  /],
		'emserr' => [qw/
			     EMS__NOMSG
			     /],
		'sai'    => [qw/
			     SAI__ERROR SAI__OK SAI__WARN
			     /]
	       );

Exporter::export_tags('ems','emserr','sai');

'$Revision$ ' =~ /.*:\s(.*)\s\$/ && ($VERSION = $1);


bootstrap Starlink::EMS $VERSION;

1;
__END__


=head1 NAME

Starlink::EMS - Perl extension for Starlink EMS library

=head1 SYNOPSIS

  use Starlink::EMS;
  ems1_get_facility_error($status, $facility, $ident, $text);
  $ok = SAI__OK;


=head1 DESCRIPTION

This module provides a simple interface to the Starlink EMS library.
All C routines are available plus an extra internal routien
for converting a status code directly to the error mesage
components (ems1_get_facility_error).

=head1 CONSTANTS

The following constants are available: SAI__OK, SAI__ERROR,
SAI__WARN. They are exported by default.

=head1 REVISION

$Id$


=head1 AUTHOR

Tim Jenness (t.jenness@jach.hawaii.edu)

=head1 SEE ALSO

perl(1).

=cut
