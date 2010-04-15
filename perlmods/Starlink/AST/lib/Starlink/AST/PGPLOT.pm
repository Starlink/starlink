package Starlink::AST::PGPLOT;

use strict;
use vars qw/ $VERSION /;
use constant R2D     => 57.29578;        # Radians to degrees factor
use constant FLT_MAX => 3.40282347e+38;  # Maximum float on ix86 platform

use PGPLOT;
use Starlink::AST;
use Carp;

$VERSION = '2.00';

=head1 NAME

Starlink::AST::PGPLOT - AST wrapper to the PGPLOT library

=head1 SYNOPSIS

   use Starlink::AST::PGPLOT

The main methods which need to be registered with the AST package
are shown below,

   $status = _GFlush();
   $status = _GLine( \@x, \@y );
   $status = _GMark( \@x, \@y, $type );
   $status = _GText( $text, $x, $y, $just, $upx, $upy );
   ( $status, $xb, $yb ) = _GTxExt( $text, $x, $y, $just, $upx, $upy );
   ( $status, $chv, $chh ) = _GQch();
   ( $status, $old_value ) = _GAttr( $attr, $value, $prim );
   ( $status, $alpha, $beta) = _GScales();

=head1 DESCRIPTION

This file implements the low level graphics functions required by the rest
of AST, by calling suitable PGPLOT functions (the FORTRAN PGPLOT interface
is used).

This file can be used as a template for the development of similar modules
to support alternative graphics systems.

=head1 NOTES

All teh functions in this module are private, and are intended to be called
from the AST module. None of these functions should be considered to be part
of the packages public interface.

=head1 METHODS

=over 4

=item B<_GFlush>

This function ensures that the display device is up-to-date, by flushing
any pending graphics to the output device.

   my $status = _GFlush();

=cut

sub _GFlush {
   pgupdt();
   return 1;
}

=item B<_GLine>

This function displays lines joining the given positions.

   my $status = _GLine( \@x, \@y );

=cut

sub _GLine {
   my $x = shift;
   my $y = shift;

   if( scalar(@$x) > 1 && scalar(@$x) == scalar(@$y) ) {
      pgline( scalar(@$x), $x, $y );
   }
   return 1;
}

=item B<_GMark>

This function displays markers at the given positions.

   my $status = _GMark( \@x, \@y, $type );

where $type is an integer used to indicate the type of marker required.

=cut

sub _GMark {
   my $x = shift;
   my $y = shift;
   my $type = shift;

   if( scalar(@$x) >= 1 && scalar(@$x) == scalar(@$y) ) {
      pgpt( scalar(@$x), $x, $y, $type );
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
   my ( $text, $x, $y, $just, $upx, $upy ) = @_;

   # check we have a string to print
   if( defined $text && length($text) != 0 ) {

      # validate the justifcation
      my $just1 = substr $just, 0, 1;
      my $just2 = substr $just, 1, 1;
      if ( defined $just && length($just) == 2 ) {

        # if we have a bogus justification string default it
        unless( $just1 =~ /^[TBC]/ ) {
           warn "_GText: bad vertical justification defaulting to 'C'\n";
           $just1 = "C";
        }
        unless( $just2 =~ /[LCR]/ ) {
           warn "_GText: bad horizontal justification defaulting to 'C'\n";
           $just2 = "C";
        }
      } else {
         warn "_GText: No justification string defaulting to 'CC'\n";
         $just1 = "C";
         $just2 = "C";
      }
      $just = $just1 . $just2;

      # get the axis scaling
      my ( $ret, $alpha, $beta ) = _GScales();
      return 0 if $ret == 0;

      # If either axis is reversed, reverse the supplied up-vector
      # components so that they refer to the world-coordinates axes.
      $upx = -$upx if $alpha < 0.0;
      $upy = -$upy if $beta < 0.0;

      # Get the angle between the text base-line and horizontal.
      my $angle = atan2( -$upx*$alpha, $upy*$beta)*R2D;

      # Get the fractional horizontal justification as needed by PGPLOT.
      my $fjust;
      if( $just2 eq "L" ) {
        $fjust = 0.0;
      } elsif ( $just2 eq "R" ) {
        $fjust = 1.0;
      } else {
        $fjust = 0.5;
      }

      # Unless the requested justification is "Bottom", we need to adjust
      # the supplied reference position before we use it with PGPLOT because
      # PGPLOT assumes "Bottom" justification.
      if( $just1 ne "B" ) {

         # Get the bounding box of the string. Note, only the size of the box
         # is significant here, not its position. Also note, leading and
         # trailing spaces are not included in the bounding box.
         my ( @xbox, @ybox );
         pgqtxt( $x, $y, $angle, $fjust, $text, \@xbox, \@ybox );

         # Normalise the up-vector in world coordinates.
         my $uplen = sqrt( $upx*$upx + $upy*$upy );
         if( $uplen > 0.0 ){
            $upx /= $uplen;
            $upy /= $uplen;
         } else {
            ReportGrfError("_GText: Zero length up-vector supplied.");
            return 0;
         }

         # Find the height of the text above the base-line. Note, the PGPLOT
         # manual is not clear about the order of the corners returned by
         # pgqtxt, so we have to find the largest distance between the corners
         # in the direction of the supplied up-vector.
         my $hu = 0.0;
         for my $i ( 0 ... 3 ) {
            my $test = $upx*( $xbox[$i] - $x ) + $upy*( $ybox[$i] - $y );
            $hu = $test if $test > $hu;
         }

         # Adjust the vertical position of the reference point, since PGPLOT
         # requires it to be at the bottom of the text.
         if( $just1 eq 'T' ) {
            $x -= $upx*$hu;
            $y -= $upy*$hu;
         } elsif( $just1 eq 'C' ){
            $x -= 0.5*$upx*$hu;
            $y -= 0.5*$upy*$hu;
         }
      }

      # Display the text, erasing any graphics.
      my $tbg;
      pgqtbg( $tbg );
      pgstbg( 0 );
      pgptxt( $x, $y, $angle, $fjust, $text );
      pgstbg( $tbg );
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

   my ( $status, $alpha, $beta ) = _GScales()

=cut

sub _GScales {
    my $alpha = shift;
    my $beta = shift;

    my ( $nx1, $nx2, $ny1, $ny2, $wx1, $wx2, $wy1, $wy2, $ret );
    pgqvp( 2, $nx1, $nx2, $ny1, $ny2 );
    pgqwin( $wx1, $wx2, $wy1, $wy2 );

    if( $wx2 != $wx1 && $wy2 != $wy1 && $nx2 != $nx1 && $ny2 != $ny1 ) {
       $alpha = ( $nx2 - $nx1 ) / ( $wx2 - $wx1 );
       $beta = ( $ny2 - $ny1 ) / ( $wy2 - $wy1 );
       $ret = 1
    } else {
       ReportGrfError("_GScales: The graphics window has zero size\n");
       $ret = 0;
    }
    return ( $ret, $alpha, $beta );
}


=item B<_GTxExt>

This function returns the corners of a box which would enclose the
supplied character string if it were displayed using astGText. The
returned box INCLUDES any leading or trailing spaces.

   my ( $status, $xb, $yb ) = _GTxtExt( $text, $x, $y, $just, $upx, $upy);

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
   my ( $text, $x, $y, $just, $upx, $upy ) = @_;

   # initalise @$xb and @$yb
   my ( @xb, @yb );
   foreach my $i ( 0 ... 3 ) {
      $xb[$i] = 0.0;
      $yb[$i] = 0.0;
   }

   # check we have a string to print
   if( defined $text && length($text) != 0 ) {

      # validate the justifcation
      my $just1 = substr $just, 0, 1;
      my $just2 = substr $just, 1, 1;
      if ( defined $just && length($just) == 2 ) {

        # if we have a bogus justification string default it
        unless( $just1 =~ /[TBC]/ ) {
           warn "_GText: bad vertical justification defaulting to 'C'\n";
           $just1 = "C";
        }
        unless( $just2 =~ /[LCR]/ ) {
           warn "_GText: bad horizontal justification defaulting to 'C'\n";
           $just2 = "C";
        }
      } else {
         warn "_GText: No justification string defaulting to 'CC'\n";
         $just1 = "C";
         $just2 = "C";
      }
      $just = $just1 . $just2;

      # get the axis scaling
      my ( $ret, $alpha, $beta ) = _GScales();
      return ( 0 ) if $ret == 0;

      # If either axis is reversed, reverse the supplied up-vector
      # components so that they refer to the world-coordinates axes.
      $upx = -$upx if $alpha < 0.0;
      $upy = -$upy if $beta < 0.0;

      # convert the up-vector into millimetres
      my $ux = $alpha*$upx;
      my $uy = $beta*$upy;

      # normalise the up-vector to a length of 1 millimetre
      my $uplen = sqrt( $ux*$ux + $uy*$uy );
      if ( $uplen > 0.0 ) {
         $ux /= $uplen;
         $uy /= $uplen;
      } else {
         ReportGrfError("_GTxtExt: Zero length up-vector supplied.");
         return ( 0 );
      }

      # Form the base-line vector by rotating the up-vector by 90 degrees
      # clockwise.
      my $vx = $uy;
      my $vy = -$ux;

      # Get the angle between the text base-line and horizontal.
      my $angle = atan2( $vy, $vx )*R2D;

      # Get the bounding box of the string drawn with its bottom left corner
      # at the origin.
      my ( @xbox, @ybox );
      pgqtxt( 0.0, 0.0, $angle, 0.0, $text, \@xbox, \@ybox );

      # Convert the returned bounding box world coordinates into millimetres.
      for my $i ( 0 ... 3 ){
         $xbox[ $i ] *= $alpha;
         $ybox[ $i ] *= $beta;
      }

      # Find the height of the bounding box, in millimetres. Note,
      # the PGPLOT manual is not clear about the order of the corners
      # returned by pgqtxt, so we have to find the largest distance between
      # the corners in the direction of the supplied up-vector. The reference
      # point is on the text base-line which is not usually at the bottom of
      # the bounding box (some letters - like "y" - extend below the base-line).
      # Find the distance from the base-line to the top (hu) and bottom (hd)
      # of the bounding box.
      my $hu = -(FLT_MAX);
      my $hd = FLT_MAX;
      foreach my $i ( 0 ... 3 ) {
         my $test = $ux*$xbox[ $i ] + $uy*$ybox[ $i ];
         $hu = $test if $test > $hu;
         $hd = $test if $test < $hd;
      }

      # Get an up and a down vector scaled to the height/depth of the
      # bounding box above/below the text base-line .
      my $uxu = $ux*$hu;
      my $uyu = $uy*$hu;
      my $uxd = $ux*$hd;
      my $uyd = $uy*$hd;

      # The bounding box returned by pgqtxt does not include any leading or
      # trailing spaces. We need to include such spaces in the returned box.
      # To do this we get the length of the text string in millimetres
      # using pglen instead of using the bounding box returned by pgqtxt.
      my ( $xl, $yl );
      pglen( 2, $text, $xl, $yl );

      # The abolute width of the string in millimetres may depend on the
      # up-vector. The values returned by pglen are for horizontal and
      # vertical text. Find the width using the supplied up-vector.
      my $a = $uy*$xl;
      my $b = $ux*$yl;
      my $width = sqrt( $a*$a + $b*$b );

      # The pglen function returns a value which is slightly smaller than
      # the area cleared to hold the text when written using pgptxt. Increase
      # the text width so that it is about equal to the area cleared.
      $width += 0.2*$hu;

      # Scale the base-line vector so that its length is equal to the width
      # of the bounding box (including spaces).
      $vx *= $width;
      $vy *= $width;

      # Convert the base-line vector back into world coordinates.
      $vx /= $alpha;
      $vy /= $beta;

      # Convert the up and down vectors into world coordinates.
      $uxu /= $alpha;
      $uyu /= $beta;
      $uxd /= $alpha;
      $uyd /= $beta;

      # Find the coordinates at the centre of the bounding box in world
      # coordinates.
      my $xc = $x;
      my $yc = $y;

      if( $just1 eq 'B' ) {
         $xc += 0.5*$uxu;
         $yc += 0.5*$uyu;
      } elsif( $just1 eq 'T' ) {
         $xc -= 0.5*$uxu;
         $yc -= 0.5*$uyu;
      }

      if( $just2 eq 'L' ) {
         $xc += 0.5*$vx;
         $yc += 0.5*$vy;
      } elsif( $just2 eq 'R' ) {
         $xc -= 0.5*$vx;
         $yc -= 0.5*$vy;
      }

      # Get the corners of the bounding box.
      my $vdx = 0.5*$vx;
      my $vdy = 0.5*$vy;
      my $udx = 0.5*$uxu;
      my $udy = 0.5*$uyu;

      # Bottom left corner...
      $xb[ 0 ] = $xc - $vdx - $udx + $uxd;
      $yb[ 0 ] = $yc - $vdy - $udy + $uyd;

      # Bottom right corner...
      $xb[ 1 ] = $xc + $vdx - $udx + $uxd;
      $yb[ 1 ] = $yc + $vdy - $udy + $uyd;

      # Top right corner...
      $xb[ 2 ] = $xc + $vdx + $udx;
      $yb[ 2 ] = $yc + $vdy + $udy;

      # Top left corner...
      $xb[ 3 ] = $xc - $vdx + $udx;
      $yb[ 3 ] = $yc - $vdy + $udy;

   }

   # Return
   return (1, \@xb, \@yb );

}

=item B<_GQch>

This function returns the heights of characters drawn vertically and
horizontally in world coordinates.

   my ( $status, $chv, $chh ) = _GQch( );

Where $chv is the height of characters drawn with a vertical
baseline. This will be an increment in the X axis.  Where $chh is the
height of characters drawn with a horizontal baseline. This will be an
increment in the Y axis.

=cut

sub _GQch {
   # return variables
   my ( $status, $chv, $chh );

   # local variables
   my ( $vx1, $vx2, $vy1, $vy2, $wx1, $wx2, $wy1, $wy2);

   # Get the character height in normalised device coordinates
   pgqcs( 0, $chv, $chh );

   # Get the bounds of the PGPLOT viewport in normalised device
   # coordinates.
   pgqvp( 0, $vx1, $vx2, $vy1, $vy2 );

   # Get the bounds of the PGPLOT window in world coordinates.
   pgqwin( $wx1, $wx2, $wy1, $wy2 );

   # Convert the text height from normalised device coordinates into world
   # coordinates for vertical text. Print an error message if the viewport
   # has zero size.
   if( $vx1 != $vx2 ){
      $chv *= ( $wx2 - $wx1 )/( $vx2 - $vx1 );
   } else {
      ReportGrfError("_GQch: The graphics viewport has zero size in the X direction.");
      return (0);
   }

   # Convert the text height from normalised device coordinates into world
   # coordinates for horizontal text. Print an error message if the viewport
   # has zero size.
   if( $vy1 != $vy2 ){
      $chh *= ( $wy2 - $wy1 )/( $vy2 - $vy1 );
   } else {
      ReportGrfError("_GQch: The graphics viewport has zero size in the Y direction.");
      return (0);
   }

   # Return.
   return ( 1, $chv, $chh );
}


=item B<_GAttr>

This function returns the current value of a specified graphics
attribute, and optionally establishes a new value. The supplied
value is converted to an integer value if necessary before use.


   my ( $status, $old_value ) = _GAttr( $attr, $value, $prim );

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
   my $attr = shift;
   my $value = shift;
   my $prim = shift;

   my ( $ival, $rval, $dx, $dy, $deflw, $x1, $x2, $y1, $y2 );
   my $old_value = undef;

   # If required retrieve the current line style, and set a new line style.
   if( $attr == Starlink::AST::Grf::GRF__STYLE() ){
      pgqls( $ival );
      $old_value = $ival;

      if( $value != Starlink::AST::AST__BAD() ){
         $ival = int( $value + 0.5 );
         $ival -= 1 if $value < 0.0;

         $ival = ( $ival - 1 ) % 5;
         $ival += ( $ival < 0 ) ? 6 : 1;

         pgsls( $ival );
      }

   # If required retrieve the current line width, and set a new line width.
   # Line width is stored in Plot as a scale factor (1.0 for the default line
   # width which is a fixed fraction of the diagonal of the view surface), but
   # pgplot stores it in units of 0.005 of an inch.
   } elsif( $attr == Starlink::AST::Grf::GRF__WIDTH() ){

      # Get the bounds of the view surface in inches.
      pgqvsz( 1, $x1, $x2, $y1, $y2 );

      # Find the default line width in inches (i.e. 0.0005 of the length
      # of the view surface diagonal).
      $dx = ( $x1 - $x2 );
      $dy = ( $y1 - $y2 );
      $deflw = 0.0005*sqrt( $dx*$dx + $dy*$dy );

      # Get the current pgplot line width in units of 0.005 of an inch.
      pgqlw( $ival );

      # If required, return the factor by which this exceeds the default line
      # width found above.
      $old_value = $ival/( 200.0 * $deflw );

      # If a new line width has been provided, the pgplot line width needs to
      # be set to the corresponding absolute value.
      if( $value != Starlink::AST::AST__BAD() ){
         $ival = 200.0*$value*$deflw;
         if( $ival < 1 ) {
            $ival = 1;
         } elsif( $ival > 201 ){
            $ival = 201;
         }
         pgslw( $ival );
      }

   # If required retrieve the current character size, and set a new size.
   # The attribute value should be a factor by which to multiply the
   # default character size.
   } elsif( $attr == Starlink::AST::Grf::GRF__SIZE() ){
      pgqch( $rval );
      $old_value = $rval;

      if( $value != Starlink::AST::AST__BAD() ){
         pgsch( $value );
      }

   # If required retrieve the current character font, and set a new font.
   } elsif( $attr == Starlink::AST::Grf::GRF__FONT() ){
      pgqcf( $ival );
      $old_value = $ival;

      if( $value != Starlink::AST::AST__BAD() ){
         $ival = int( $value + 0.5 );
         $ival -= 1 if $value < 0.0;

         $ival = ( $ival - 1 ) % 4;
         $ival += ( $ival < 0 ) ? 5 : 1;
         pgscf( $ival );
      }

   # If required retrieve the current colour index, and set a new colour
   # index.
   } elsif( $attr == Starlink::AST::Grf::GRF__COLOUR() ){
      pgqci( $ival );
      $old_value = $ival;

      if( $value != Starlink::AST::AST__BAD() ){
         $ival = int( $value + 0.5 );
         $ival = 1 if $ival < 0;
         pgsci( $ival );
      }

   # Give an error message for any other attribute value.
   } else {
      ReportGrfError("_GAttr: Unknown graphics attribute $attr requested.");
      return ( 0 );
   }

   # Return.
   return ( 1, $old_value );

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

  if ($cap == &Starlink::AST::Grf::GRF__SCALES) {
    return 1;
  }
  return 0;
}


# Internal error setting routine
sub ReportGrfError {
  my $text = shift;
  warn "Generated AST error in perl PGPLOT callback: $text\n";
  Starlink::AST::_Error( &Starlink::AST::Status::AST__GRFER(), $text);
}


=back

=head1 COPYRIGHT

Copyright (C) 2004 Particle Physics and Astronomy Research Council.
Copyright (C) 2004 University of Exeter. All Rights Reserved.

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU Public License.

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

=head1 AUTHORS

Alasdair Allan E<lt>aa@astro.ex.ac.ukE<gt>,
Tim Jenness E<lt>tjenness@jach.hawaii.eduE<gt>

=cut

package Starlink::AST::Plot;

use strict;
use vars qw/ $VERSION /;

use Starlink::AST::PGPLOT;

sub pgplot {
  my $self = shift;

  $self->GFlush(\&Starlink::AST::PGPLOT::_GFlush);
  $self->GLine(\&Starlink::AST::PGPLOT::_GLine);
  $self->GMark(\&Starlink::AST::PGPLOT::_GMark);
  $self->GText(\&Starlink::AST::PGPLOT::_GText);
  $self->GTxExt(\&Starlink::AST::PGPLOT::_GTxExt);
  $self->GQch(\&Starlink::AST::PGPLOT::_GQch);
  $self->GAttr(\&Starlink::AST::PGPLOT::_GAttr);
  $self->GScales(\&Starlink::AST::PGPLOT::_GScales);
  $self->GCap(\&Starlink::AST::PGPLOT::_GCap);

  return 1;
}

1;
