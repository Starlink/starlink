package Starlink::AST;

use strict;
use Carp;

use vars qw/ $VERSION /;

require DynaLoader;
use base qw| DynaLoader |;

$VERSION = '0.01';

bootstrap Starlink::AST $VERSION;

1;

=head1 NAME

Starlink::AST - Interface to the Starlink AST library

=head1 SYNOPSIS


=head1 DESCRIPTION

=head1 AUTHOR

Tim Jenness E<lt>t.jenness@jach.hawaii.eduE<gt>

Copyright (C) 2004 Tim Jenness. All Rights Reserved.



=cut

# All the inheritance stuff

package AstObjectPtr;


# Provide the sprintf functionality in the Perl side since it
# is easier than doing it in C

sub Set {
  my $self = shift;
  my $string = shift;
  # token substitution if we have more arguments
  $string = sprintf($string, @_) if @_;
  return $self->_Set( $string );
}

package AstAxisPtr;
use base qw/ AstObjectPtr /;

package AstSkyAxisPtr;
use base qw/ AstAxisPtr /;

package AstChannelPtr;
use base qw/ AstObjectPtr /;

package AstFitsChanPtr;
use base qw/ AstChannelPtr /;

package AstXmlChanPtr;
use base qw/ AstChannelPtr /;

package AstMappingPtr;
use base qw/ AstObjectPtr /;

package AstCmpMapPtr;
use base qw/ AstMappingPtr /;

package AstDssMapPtr;
use base qw/ AstMappingPtr /;

package AstLutMapPtr;
use base qw/ AstMappingPtr /;

package AstGrismMapPtr;
use base qw/ AstMappingPtr /;

package AstIntraMapPtr;
use base qw/ AstMappingPtr /;

package AstMathMapPtr;
use base qw/ AstMappingPtr /;

package AstMatrixMapPtr;
use base qw/ AstMappingPtr /;

package AstPcdMapPtr;
use base qw/ AstMappingPtr /;

package AstPermMapPtr;
use base qw/ AstMappingPtr /;

package AstPolyMapPtr;
use base qw/ AstMappingPtr /;

package AstShiftMapPtr;
use base qw/ AstMappingPtr /;

package AstSlaMapPtr;
use base qw/ AstMappingPtr /;

package AstSpecMapPtr;
use base qw/ AstMappingPtr /;

package AstUnitMapPtr;
use base qw/ AstMappingPtr /;

package AstWcsMapPtr;
use base qw/ AstMappingPtr /;

package AstWinMapPtr;
use base qw/ AstMappingPtr /;

package AstZoomMapPtr;
use base qw/ AstMappingPtr /;

package AstFramePtr;
use base qw/ AstMappingPtr /;

package AstFrameSetPtr;
use base qw/ AstFramePtr /;

package AstCmpFramePtr;
use base qw/ AstFramePtr /;

package AstSkyFramePtr;
use base qw/ AstFramePtr /;

package AstSprecFramePtr;
use base qw/ AstFramePtr /;



1;
