package Starlink::EMS;

use strict;
use Carp;
use vars qw($VERSION @ISA @EXPORT );

require Exporter;
require DynaLoader;

@ISA = qw(Exporter DynaLoader);

# Export the main routine
@EXPORT = qw(
	     ems1_get_facility_error
	     SAI__OK SAI__ERROR SAI__WARN
);
$VERSION = '0.03';


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
It provides a single function for converting Starlink error status
integers into error texts and names.

The only routine exported by this module is ems1_get_facility_error.

=head1 CONSTANTS

The following constants are available: SAI__OK, SAI__ERROR,
SAI__WARN. They are exported by default.

=head1 AUTHOR

Tim Jenness (t.jenness@jach.hawaii.edu)

=head1 SEE ALSO

perl(1).

=cut
