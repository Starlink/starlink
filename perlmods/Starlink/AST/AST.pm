package Starlink::AST;

use strict;
use Carp;

use vars qw/ $VERSION /;

require DynaLoader;
use base qw| DynaLoader |;


$VERSION = '0.01';

bootstrap Starlink::AST $VERSION;

=head1 NAME

Starlink::AST - Interface to the Starlink AST library

=head1 SYNOPSIS

  use Starlink::AST;

  my $zmap = new Starlink::AST::ZoomMap( 2, 5, "" );
  $zmap->Set( Zoom => 5 );

  my ($xout, $yout ) = $zmap->Tran2( [1,2], [4,6], 1);

  my $fchan = new Starlink::AST::FitsChan();
  for (<DATA>) {
   $fchan->PutFits( $_, 0);
  }
  $fchan->Clear( "Card" );

  $wcs = $fchan->Read();

=head1 DESCRIPTION

C<Starlink::AST> provides a perl wrapper to the Starlink AST library.
The Starlink AST library provides facilities for transforming coordinates
from one system to another in an object oriented manner. Multiple coordinate
frames can be associated with a data set and it is also possible to generate
automatic mappings between frames.

Coordinate frame objects can be imported from FITS headers and from NDF files.

=head1 CALLING CONVENTIONS

In general the method names used in the Perl interface match the
function names in the C library with the "ast" prefix
dropped. Functions that require C arrays, should take references to
Perl arrays. AST functions that return values/arrays now return these
values/arrays onto the perl stack rather than the argument stack.

The constructor functions are now replaced with C<new> methods in the
relevant class. e.g rather than calling astZoomMap(), the Perl
interface uses the C<new> method in the C<Starlink::AST::ZoomMap>
namespace.

=head2 Constructors

The following constructors are available. Currently, these
constructors match the C constructors fairly closely. 

This is one area which may change when the class comes out of alpha
release. The main problem with the constructors is the options string
(a standard AST option string with comma-separated keyword value
pairs). It would make more sense to replace these constructors with
hash constructors that take the mandatory arguments in the correct
order and hash arguments for the options.

=over 4

=item B<Starlink::AST::Frame>

Instantiate an astFrame() object.

  $frame = new Starlink::AST::Frame( $naxes, $options );

=item B<Starlink::AST::FrameSet>

  $frameSet = new Starlink::AST::FrameSet( $frame, $options );

=item B<Starlink::AST::CmpFrame>

  $cmpFrame = new Starlink::AST::CmpFrame( $frame1, $frame2, $options );

=item B<Starlink::AST::CmpMap>

  $cmpMap = new Starlink::AST::CmpMap( $map1, $map2, $series, $options );

=item B<Starlink::AST::Channel>

The astChannel contructor takes a hash argument. There are no
mandatory keys to the hash. Sink and Source callbacks for the channel
can be supplied using the keys "sink" and "source". All other keys are
expected to correspond to attributes of the channel object
(e.g. Comment, Full and Skip for astChannel).

  $chann = new Starlink::AST::Channel( %options );

  $chann = new Starlink::AST::Channel( sink => sub { print "$_[0]\n"; } );

The "sink" callback expects to be given a single argument as a string.
The "source" callback takes no arguments and should return a single string.

=item B<Starlink::AST::FitsChan>

Same calling signature as C<Starlink::AST::Channel>.

=item B<Starlink::AST::XmlChan>

Same calling signature as C<Starlink::AST::Channel>. Note that xmlChan
is only available for AST v3.1 and newer.

=item B<Starlink::AST::GrisMap>

Only available in AST v3.0 and newer.

  $grismMap = new Starlink::AST::GrisMap( $options );

=item B<Starlink::AST::IntraMap>

Not Yet Implemented.

 $intraMap = new Starlink::AST::IntraMap( $name, $nin, $nout, $options );

=back

=head2 Base class methods

These methods will work on all classes of AST objects.



=head2 Mapping methods

=head2 FrameSet methods

=head1 EXCEPTIONS

Rather than using the C<astOK> function provided to the C interface (which
is not thread safe) AST errors are converted to Perl exceptions which can
be caught with an eval.

=head1 TODO

 + Convert AST croaks to true exceptions of class Starlink::AST::Error

 + Provide interface to the plotting routines

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

# Channels need a wrapper

package Starlink::AST::Channel;

sub new {
  # This should work for FitsChan and Channel and XmlChan
  my $class = shift;
  my %args = @_;

  my %processed;

  # sink and source are special. All others are attributes
  if (exists $args{sink} ) {
    $processed{sink} = $args{sink};
    delete $args{sink};
  }
  if (exists $args{source} ) {
    $processed{source} = $args{source};
    delete $args{source};
  }

  # Convert all remaining options to comma separated string
  my @options;
  for my $k (keys %args ) {
    push(@options, "$k=$args{$k}");
  }
  $processed{options} = join(",",@options) if @options;

  # Call the underlying routine
  $class->_new( \%processed );

}

package Starlink::AST::FitsChan;
use base qw/ Starlink::AST::Channel /;

package Starlink::AST::XmlChan;
use base qw/ Starlink::AST::Channel /;


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

# Looks like AST is smart enough to figure out whether we want to SetC(),
# SetD() or SetI() all on its own, ditto for GetC(), GetD() or GetI(). We
# can therfore write generic Set() and Get() perl wrapper that ignore the
# underlying strict typing (and also take hash/list as areguements). This
# is much more Perl like than would otherwise be the case.
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
  
  if ( $_[0] =~ "=" ) {
     $self->_Set( $_[0] );
  } else {
     my %hash = @_;
     foreach my $key ( sort keys %hash ) {
        $self->SetC( $key, $hash{$key} );
     }   
  }
  return;
  
}

sub Get {
  my $self = shift;
  my @strings = @_;
  
  my %hash;
  foreach my $i ( 0 ... $#strings ) {
     $hash{$strings[$i]} = $self->GetC( $strings[$i] );
  }  
  return wantarray ? %hash : $hash{$strings[0]};
    
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

package AstPlotPtr;
use base qw/ AstFrameSetPtr /;

package AstCmpFramePtr;
use base qw/ AstFramePtr /;

package AstSkyFramePtr;
use base qw/ AstFramePtr /;

package AstSpecFramePtr;
use base qw/ AstFramePtr /;



1;
