package Starlink::AST;

use strict;
use Carp;

use vars qw/ $VERSION /;

require DynaLoader;
use base qw| DynaLoader |;
use Scalar::Util;

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

This program is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free Software
Foundation; either version 2 of the License, or (at your option) any later
version.

This program is distributed in the hope that it will be useful,but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
PARTICULAR PURPOSE. See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with
this program; if not, write to the Free Software Foundation, Inc., 59 Temple
Place,Suite 330, Boston, MA  02111-1307, USA

=cut

# Exception handling

package Starlink::AST::Status;

# This is called via the ASTCALL C macro
# Arguments are : status value and a reference to an array
# containing the message stack

sub ThrowError {
  my $status = shift;
  my $err = shift;
  my $str = join("\n",map { "- $_" } @$err) . "\n";
  # This should throw an appropriate exception
  # for now just croak
  Carp::croak( $str );
}

# All the inheritance stuff

package AstObjectPtr;

sub Set {
  my $self = shift;
  
  # Original code, using the lower lever _Set method. Provide the sprintf 
  # functionality in the Perl side since it is easier than doing it in C 
  # [but causes a problem if the string includes a comma]
  #
  #my $string = shift;
  #
  # token substitution if we have more arguments
  #
  #$string = sprintf($string, @_) if @_;
  #return $self->_Set( $string );
  
  my %hash = @_;
  foreach my $key ( sort keys %hash ) {
     $self->SetC( $key, $hash{$key} );
  }   
  return;
  
}

sub Get {
  my $self = shift;
  my $string = shift;
  
  my $value = $self->GetC( $string );
  return $value;
}

# Rebless cloned/copied object into the original class
sub Clone {
  my $self = shift;
  my $new = $self->_Clone();
  return bless $new, ref($self);
}

sub Copy {
  my $self = shift;
  my $new = $self->_Clone();
  return bless $new, ref($self);
}

package AstAxisPtr;
use base qw/ AstObjectPtr /;

package AstSkyAxisPtr;
use base qw/ AstAxisPtr /;

package AstChannelPtr;
use base qw/ AstObjectPtr /;

# Need to rebless objects obtained from an astRead into the
# correct class rather than generic variant.

sub Read {
  my $self = shift;
  my $new = $self->_Read();
  return if !defined $new;
  return if !Starlink::AST::OK();
  my $ast_class = $new->GetC( "Class" );
  my $perl_class = "Ast" . $ast_class . "Ptr";
  return bless $new, $perl_class;
}


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

package AstSphMapPtr;
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
