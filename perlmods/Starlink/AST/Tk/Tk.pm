package Starlink::AST::Tk;

use strict;
use vars qw/ $VERSION /;
use constant R2D     => 57.29578;        # Radians to degrees factor
use constant FLT_MAX => 3.40282347e+38;  # Maximum float on ix86 platform

use Tk;
use Starlink::AST;
use Carp;

'$Revision$ ' =~ /.*:\s(.*)\s\$/ && ($VERSION = $1);

=head1 NAME

Starlink::AST::Tk - AST wrapper to the Tk library

=head1 SYNOPSIS

   use Starlink::AST::Tk

The main methods which need to be registered with the AST package
are shown below,

   $status = _GFlush( $w );
   $status = _GLine( $w, \@x, \@y );
   $status = _GMark( $w, \@x, \@y, $type );
   $status = _GText( $w, $text, $x, $y, $just, $upx, $upy );
   ( $status, $xb, $yb ) = _GTxtExt( $w, $text, $x, $y, $just, $upx, $upy );
   ( $status, $chv, $chh ) = _GQch( $w );
   ( $status, $old_value ) = _GAttr( $w, $attr, $value, $prim );

The following helper methods are also provided,

   my ( $status, $alpha, $beta ) = _GScales( $w )

=head1 DESCRIPTION
  
This file implements the low level graphics functions required by the rest
of AST, by calling suitable Tk::Canvas functions. In all the routines $w
is a reference to the Tk::Canvas object on which we're plotting.

=head1 NOTES

All the functions in this module are private, and are intended to be called
from the AST module. None of these functions should be considered to be part
of the packages public interface.

=head1 REVISION

$Id$

=head1 METHODS

=over 

=item B<_GFlush>

This function ensures that the display device is up-to-date, by flushing 
any pending graphics to the output device.

   my $status = _GFlush( $w );

=cut

sub _GFlush {
   my $external = shift;   
   my $canvas = $$external[0];
   $canvas->update();
   return 1;
}

=item B<_GLine>

This function displays lines joining the given positions.

   my $status = _GLine( $w, \@x, \@y );

=cut

sub _GLine {
   my ( $external, $xf, $yf ) = @_;
   my $canvas = $$external[0];
   my ($xlo,$xhi,$ylo,$yhi) = @$external[1 .. 4];
   
   if( scalar(@$xf) > 1 && scalar(@$xf) == scalar(@$yf) ) {
      
      my $xmax = $canvas->cget( '-width' );
      my $xmin = 0 + ($xlo*$xmax);

      my $ymax = $canvas->cget( '-height' );
      my $ymin = 0 + ($ylo*$ymax);
       
      my ( @x, @y, @points);
      foreach my $i ( 0 ... $#$xf ) {
         
         $x[$i] = $$xf[$i]*$xmax;
         $y[$i] = (1 - $$yf[$i])*$ymax;   
         push @points, $x[$i];
         push @points, $y[$i];
         #print "\nPOINT $i\nXF = $$xf[$i], YF = $$yf[$i]\n";
         #print "X[$i] = $x[$i], Y = $y[$i]\n";
         #print "XLO = $xlo, XHI $xhi, YLO = $ylo, YHI = $yhi\n";
         #print "XMAX = $xmax, YMAX = $ymax\n";
      }
      
      $canvas->createLine( @points );
   }   
   return 1;
}

=item B<_GMark>

This function displays markers at the given positions.

   my $status = _GMark( $w, \@x, \@y, $type );

where $type is an integer used to indicate the type of marker required.

=cut

sub _GMark {
   my ($external, $xf, $yf, $type) = @_;
   my $canvas = $$external[0];
   my ($xlo,$xhi,$ylo,$yhi) = @$external[1 .. 4];
   
   if( scalar(@$xf) > 1 && scalar(@$xf) == scalar(@$yf) ) {
      
      my $xmax = $canvas->cget( '-width' );
      my $xmin = 0 + ($xlo*$xmax);

      my $ymax = $canvas->cget( '-height' );
      my $ymin = 0 + ($ylo*$ymax);
        
      my ( @x, @y, @points);
      foreach my $i ( 0 ... $#$xf ) {
         $x[$i] = $$xf[$i]*$xmax;
         $y[$i] = (1 - $$yf[$i])*$ymax;   
                  
         # basic scaling factor
         my $scale = $xmax/500;
         
         # scaling for rectangles
         if ( $type == 0 || $type == 6 ) {
           $scale = $scale;
         } elsif ( $type == 19 ) {
           $scale = 2*$scale;
           
         # scaling fpor circles  
         } elsif ( $type == 20 ) {
           $scale = $scale; 
         } elsif ( $type == 21 ) {
           $scale = 2*$scale; 
         } elsif ( $type == 22 ) {
           $scale = 3*$scale; 
         } elsif ( $type == 23 ) {
           $scale = 4*$scale; 
         } elsif ( $type == 24 ) {
           $scale = 5*$scale; 
         } elsif ( $type == 25 ) {
           $scale = 6*$scale; 
         } elsif ( $type == 26 ) {
           $scale = 7*$scale; 
         } elsif ( $type == 27 ) {
           $scale = 9*$scale; 
         }    
         
         my $x1 = $x[$i] - $scale*Starlink::AST::Grf::GRF__SIZE();
         my $y1 = $y[$i] - $scale*Starlink::AST::Grf::GRF__SIZE();
         my $x2 = $x[$i] + $scale*Starlink::AST::Grf::GRF__SIZE();
         my $y2 = $y[$i] + $scale*Starlink::AST::Grf::GRF__SIZE(); 

         # RECTANGLE
         if ( ($type == 0) || ($type == 6) || ($type == 19) ) {
            $canvas->createRectangle( $x1, $y1, $x2, $y2 );
         
         # CIRCLE
         } if ( $type >= 20 && $type <= 27 ) {
            $canvas->createOval( $x1, $y1, $x2, $y2 );
         
         }

      }
   }
   return 1;
}

=item B<_GText>

This function displays a character string $text at a given position using 
a specified justification and up-vector.

   my $status = _GText( $text, $x, $y, $just, $upx, $upy );

where $x is the reference x coordinate, $y is the reference y coordinate, 
and where $just is a character string which specifies the location within
the text string which is to be placed at the reference position given by x
and y. The first character may be 'T' for "top", 'C' for "centre", or 'B'
for "bottom", and specifies the vertical location of the reference position.
Note, "bottom" corresponds to the base-line of normal text. Some characters 
(eg "y", "g", "p", etc) descend below the base-line. The second  character
may be 'L' for "left", 'C' for "centre", or 'R'  for "right", and specifies
the horizontal location of the  reference position. If the string has less
than 2 characters then 'C' is used for the missing characters.

And $upx is the x component of the up-vector for the text, in graphics
world coordinates. If necessary the supplied value should be negated to
ensure that positive values always refer to displacements from  left to
right on the screen.

While $upy is the y component of the up-vector for the text, in graphics
world coordinates. If necessary the supplied value should be negated to
ensure that positive values always refer to displacements from  bottom to
top on the screen.

=cut

sub _GText {
   my ( $external, $text, $xf, $yf, $just, $upx, $upy ) = @_;
   my $canvas = $$external[0];
   my ($xlo,$xhi,$ylo,$yhi) = @$external[1 .. 4];
   print "_GText: Placeholder routine called\n";
   
   # check we have a string to print
   if( defined $text && length($text) != 0 ) {
                          
      my $xmax = $canvas->cget( '-width' );
      my $xmin = 0 + ($xlo*$xmax);

      my $ymax = $canvas->cget( '-height' );
      my $ymin = 0 + ($ylo*$ymax);
     
      # multiple the current co-ordinate
      my $x = $xf*$xmax;
      my $y = (1 - $yf)*$ymax;         
      
      # draw text
      $canvas->createText( $x, $y, -text => $text );
   }
   
   # Return, all is well strangely
   return 1;   
}            


=item B<_GScales>

This function returns two values (one for each axis) which scale
increments on the corresponding axis into a "normal" coordinate system in
which: The axes have equal scale in terms of (for instance) millimetres
per unit distance, X values increase from left to right and the Y values 
increase from bottom to top.

   my ( $status, $alpha, $beta ) = _GScales( $w )

=cut

sub _GScales {
    my ( $external, $alpha, $beta ) = @_;
    my $canvas = $$external[0];
    my ($xlo,$xhi,$ylo,$yhi) = @$external[1 .. 4];
    print "_GScales: Placeholder routine called\n";
    
    my ( $nx1, $nx2, $ny1, $ny2, $wx1, $wx2, $wy1, $wy2, $ret );
    
    $nx1 = 0;
    $nx2 = $canvas->cget( '-width' );
    $ny1 = $canvas->cget( '-height' );
    $ny2 = 0;
    
    $wx1 = $xlo;
    $wx2 = $xhi;
    $wy1 = $yhi;
    $wy2 = $ylo;    

    if( $wx2 != $wx1 && $wy2 != $wy1 && $nx2 != $nx1 && $ny2 != $ny1 ) {
       $alpha = ( $nx2 - $nx1 ) / ( $wx2 - $wx1 );
       $beta = ( $ny2 - $ny1 ) / ( $wy2 - $wy1 );
       $ret = 1
    } else {
       ReportGrfError("_GScales: The graphics window has zero size");
       return (0);
    }
    return ( $ret, $alpha, $beta );   
}       


=item B<_GTxExt>

This function returns the corners of a box which would enclose the 
supplied character string if it were displayed using astGText. The 
returned box INCLUDES any leading or trailing spaces.

   my ( $status, $xb, $yb ) = _GTxtExt( $w, $text, $x, $y, $just, $upx, $upy);

where $x is the reference x coordinate, $y is the reference y coordinate, 
and where $justification is a character string which specifies the
location within the text string which is to be placed at the reference
position given by x and y. The first character may be 'T' for "top", 'C'
for "centre", or 'B' for "bottom", and specifies the vertical location of
the reference position. Note, "bottom" corresponds to the base-line of
normal text. Some characters  (eg "y", "g", "p", etc) descend below the
base-line. The second  character may be 'L' for "left", 'C' for "centre",
or 'R'  for "right", and specifies the horizontal location of the 
reference position. If the string has less than 2 characters then 'C' is
used for the missing characters. 

And $upx is the x component of the up-vector for the text, in graphics
world coordinates. If necessary the supplied value should be negated to
ensure that positive values always refer to displacements from  left to
right on the screen.

While $upy is the y component of the up-vector for the text, in graphics
world coordinates. If necessary the supplied value should be negated to
ensure that positive values always refer to displacements from  bottom to
top on the screen.

Finally $xb is a refernce to an array of 4 elements in which to return the
x coordinate of each corner of the bounding box, and $yb is a reference to
an array of 4 elements in which to return the y coordinate of each corner
of the bounding box.

Notes:
     -  The order of the corners is anti-clockwise (in world coordinates)
        starting at the bottom left.
     -  A NULL value for "just" causes a value of "CC" to be used.
     -  Both "upx" and "upy" being zero causes an error.
     -  Any unrecognised character in "just" causes an error.
     -  Zero is returned for all bounds of the box if an error occurs.

=cut

sub _GTxExt {
   my ( $external, $text, $x, $y, $just, $upx, $upy ) = @_;
   my $canvas = $$external[0];
   my ($xlo,$xhi,$ylo,$yhi) = @$external[1 .. 4];
   print "_GTxExt: Placeholder routine called\n";
   
   # initalise @$xb and @$yb
   my ( @xb, @yb );
   $xb[0] = $x;
   $yb[0] = $y;
   $xb[1] = $x + (length($text)*12)/( $canvas->cget( '-width' ) );
   $yb[1] = $y;
   $xb[2] = $x + (length($text)*12)/( $canvas->cget( '-width' ) );
   $yb[2] = $y + 12/( $canvas->cget( '-height' ) );
   $xb[3] = $x;
   $yb[3] = $y + 12/( $canvas->cget( '-height' ) );
    
   # Return
   return (1, \@xb, \@yb );     
   
}          

=item B<_GQch>

This function returns the heights of characters drawn vertically and
horizontally in world coordinates.

   my ( $status, $chv, $chh ) = _GQch( $w );

Where $chv is a reference which is to receive the height of characters 
drawn with a vertical baseline. This will be an increment in the X axis.

Where $chh is a reference which is to receive the height of characters 
drawn with a horizontal baseline. This will be an increment in the Y axis.

=cut

sub _GQch {
   my $external = shift;
   my $canvas = $$external[0];
   my ($xlo,$xhi,$ylo,$yhi) = @$external[1 .. 4];
   print "_GQch: Placeholder routine called\n";
   
   my $chv = 12/$canvas->cget( '-height' );
   my $chh = 12/$canvas->cget( '-width' );  
   return (1, $chv, $chh );  
}   


=item B<_GAttr>

This function returns the current value of a specified graphics
attribute, and optionally establishes a new value. The supplied
value is converted to an integer value if necessary before use.


   my ( $status, $old_value ) = _GAttr( $w, $attr, $value, $prim );

Where $attr is an integer value identifying the required attribute. 
The following symbolic values are defined in the AST grf.h:

           GRF__STYLE  - Line style.
           GRF__WIDTH  - Line width.
           GRF__SIZE   - Character and marker size scale factor.
           GRF__FONT   - Character font.
           GRF__COLOUR - Colour index.

$value is a new value to store for the attribute. If this is 
AST__BAD no value is stored, and $old_value is a scalar containing
the old attribute value, if this is NULL no value is returned. 
 
Finally $prim is the sort of graphics primitive to be drawn with 
the new attribute. Identified by the following values defined in 
AST's grf.h:

           GRF__LINE
           GRF__MARK
           GRF__TEXT

=cut

sub _GAttr {
   my ( $external, $attr, $value, $prim ) = @_;
   print "_GAttr: Placeholder routine called\n";
   return ( 1, undef );
}   


=item B<_GCap>

This function is called by the AST Plot class to determine if the
grf module has a given capability, as indicated by the "cap"
argument.

  $has_cap = _GCap( $cap, $value );

The capability string should be one of the following constants
provided in the Starlink::AST::Grf namespace:

GRF__SCALES: This function should return a non-zero value if
it implements the astGScales function, and zero otherwise. The
supplied "value" argument should be ignored.

GRF__MJUST: This function should return a non-zero value if
the astGText and astGTxExt functions recognise "M" as a
character in the justification string. If the first character of
a justification string is "M", then the text should be justified
with the given reference point at the bottom of the bounding box.
This is different to "B" justification, which requests that the
reference point be put on the baseline of the text, since some
characters hang down below the baseline. If the astGText or
astGTxExt function cannot differentiate between "M" and "B",
then this function should return zero, in which case "M"
justification will never be requested by Plot. The supplied
"value" argument should be ignored.

GRF__ESC: This function should return a non-zero value if the
astGText and astGTxExt functions can recognise and interpret
graphics escape sequences within the supplied string. These
escape sequences are described below. Zero should be returned
if escape sequences cannot be interpreted (in which case the
Plot class will interpret them itself if needed). The supplied
"value" argument should be ignored only if escape sequences cannot
be interpreted by astGText and astGTxExt. Otherwise, "value"
indicates whether astGText and astGTxExt should interpret escape
sequences in subsequent calls. If "value" is non-zero then
escape sequences should be interpreted by astGText and
astGTxExt. Otherwise, they should be drawn as literal text.

Zero should be returned if the supplied capability is not recognised.

=cut

sub _GCap {
  my $cap = shift;
  my $value = shift;
   print "_GCap: Placeholder routine called [assume lack capability]\n";
  return 0;
}

# Internal error setting routine
sub ReportGrfError {
  my $text = shift;
  my $canvas = $$external[0];
  my ($xlo,$xhi,$ylo,$yhi) = @$external[1 .. 4];
  warn "Generated AST error in perl PGPLOT callback: $text\n";
  Starlink::AST::_Error( &Starlink::AST::Status::AST__GRFER(), $text);
}


=back

=head1 COPYRIGHT

Copyright (C) 2004 University of Exeter. All Rights Reserved.

This program is free software; you can redistribute it and/or modify 
it under the terms of the GNU Public License.

=head1 AUTHORS

Alasdair Allan E<lt>aa@astro.ex.ac.ukE<gt>,

=cut

package Starlink::AST::Plot;

use strict;
use vars qw/ $VERSION /;

use Starlink::AST::Tk;

sub tk {
  my $self = shift;
  my $canvas = shift;
 
  my @external;
  push @external, $canvas;
  push @external, $self->GBox();
  
  $self->GExternal( \@external );
  $self->GFlush(\&Starlink::AST::Tk::_GFlush);  
  $self->GLine(\&Starlink::AST::Tk::_GLine);
  $self->GMark(\&Starlink::AST::Tk::_GMark);
  $self->GText(\&Starlink::AST::Tk::_GText);
  $self->GTxExt(\&Starlink::AST::Tk::_GTxExt);
  $self->GQch(\&Starlink::AST::Tk::_GQch);
  $self->GAttr(\&Starlink::AST::Tk::_GAttr);
  $self->GScales(\&Starlink::AST::Tk::_GScales);
  $self->GCap(\&Starlink::AST::Tk::_GCap);


  return 1; 
}

1;
