#!/usr/bin/perl -w

#+
#  Name;
#     int.pl
#
#  Description:
#     This script generates postscript (using metapost) of a cartoon of
#     the four CCDs on the INT WFC instrument.  According to how some
#     scaling paramters are set it will give you the *exact* geometry,
#     or an idealised geometry, or an exagerrated one (or anything in
#     between), so you can see what the distortions look like.
#
#     It works using metapost.
#
#  Authors:
#     MBT: Mark Taylor (Starlink)
#
#  History:
#     24-MAR-2000 (MBT):
#        Initial version.
#-



########################################################################
#  Initial parameters.  Change these to taste.

#  Scaling and positioning on page.
      my( $scale ) = 0.03;                    # Overall size of the image
      my( $xoff, $yoff ) = ( 5000, 5000 );    # Position wrt postscript origin

#  Distortion parameters.  For all of these, a value of 0.0 will give the
#  idealised geometry (what we would like the instrument makers to have
#  done), a value of 1.0 will give the exact geometry (what we have 
#  determined is the case from careful image registration) and higher 
#  values will give an exaggerated image of the distortions.
      my( $bo ) = 1.0;    # border size
      my( $da ) = 1.0;    # angular eccentricity
      my( $dp ) = 1.0;    # positional eccentricity
      my( $dd ) = 1.0;    # nonlinear distortion

   
########################################################################
#  Geometry information for the WFC.

#  Orientation in degrees of each chip.
      my( @theta );
      @theta[ 1, 2, 3, 4 ] = (
          0 + 0.01686 * $da,
        -90 - 0.62115 * $da,
          0 + 0.11436 * $da,
          0 + 0.00000 * $da 
      );

      my( @dtheta, $i );
      foreach $i ( 1 .. 4 ) {
         $dtheta[ $i ] = $theta[ $i ] * atan2(1,0) / 90;
      }
   
#  Positioning of each chip; rather complicated the way I've parameterised
#  it, but each of the values given should evaluate to the positions
#  of the optical centre in the coordinates of the chip in question.
#  (its idealised value when $dp = 0 and its real value when $dp = 1);
      my( @cx, @cy );
      ( $cx[4], $cy[4] ) = ( 1778, 3029 );
      ( $cx[1], $cy[1] ) = ( $cx[4] - 2048 - 50 * $bo - $dp * 16.74, 
                             $cy[4] + 0 + $dp * 10.14 );
      ( $cx[2], $cy[2] ) = ( 6144 - $cy[4] + 50 * $bo + $dp * 15.68, 
                             - 50 * $bo + $dp * 14.74 + $cx[4] - $dp * 13.07 );
      ( $cx[3], $cy[3] ) = ( $cx[4] + 2048 + 50 * $bo - $dp * 0.27,  
                             $cy[4] + 0 - $dp * 32.07 );

#  Value of the nonlinear pincushion distortion coefficient.
      my( $disco ) = -5.2e-10 * $dd;

#  Function to do remapping of coordinates from pixel coordinates of a 
#  given CCD to metapost coordinates.
      sub remap {
         my( $ccd, $x, $y ) = @_;
         my( $sx ) = $x - $cx[ $ccd ];
         my( $sy ) = $y - $cy[ $ccd ];
         my( $rx ) = $sx * cos( $dtheta[ $ccd ] ) 
                   - $sy * sin( $dtheta[ $ccd ] );
         my( $ry ) = $sx * sin( $dtheta[ $ccd ] ) 
                   + $sy * cos( $dtheta[ $ccd ] );
         my( $fx ) = $rx * ( 1 + $disco * ( $rx * $rx + $ry * $ry ) );
         my( $fy ) = $ry * ( 1 + $disco * ( $rx * $rx + $ry * $ry ) );
         return( $scale * ($fx + $xoff), $scale * ($fy + $yoff) );
      }

#  Convenience function harnesses remap() and returns a metaposty pair. 
      sub pairmap {
         return sprintf( "(%.3f, %.3f)", remap( @_ ) );
      }


########################################################################
#  Build the metapost program.

#  Initialise string to hold the metapost program.
      my( $mp ) = "";
      $mp .= "prologues := 1;\n";
      $mp .= "beginfig(1);\n";
      $mp .= "defaultscale := " . $scale * 75 . ";\n";

#  Set edges of the CCDs.
      my( @lines ) = (
         [ 0, 0, 2048, 0 ], 
         [ 2048, 0, 2048, 4096 ],
         [ 2048, 4096, 0, 4096 ],
         [ 0, 4096, 0, 0 ],
      );

#  Set number of points to draw (must be >1 if the edges are curvy).
      my( $num ) = 8;


#  Draw CCD rectangles.
      foreach $i ( 1 .. 4 ) {

#  Draw the edges.
         foreach $l ( @lines ) {
            $mp .= "draw \n";
            foreach $a ( 0 .. $num ) {
               my( $px ) = $l->[0] + ( $l->[2] - $l->[0] ) * ( $a / $num );
               my( $py ) = $l->[1] + ( $l->[3] - $l->[1] ) * ( $a / $num );
               $mp .= "   " .  pairmap( $i, $px, $py ) .
                               ( ( $a < $num ) ? ".." : "" ) . "\n";
            }
            $mp .= ";\n";
         }

#  Draw a dot at the origin of the CCD.
         $mp .= "dotlabel(\"\"," . pairmap( $i, 0, 0 ) . " );\n";

#  Write a label (number) in the middle of the CCD.
         $mp .= "label(\"$i\"," . pairmap( $i, 1024, 2048 ) . ");\n";
      }

#  Little X marks the global origin.
      my( $s ) = 100;
      $mp .= "  draw " . pairmap( 4, $cx[4]-$s, $cy[4]-$s )
              . " -- " . pairmap( 4, $cx[4]+$s, $cy[4]+$s )
              . ";\n" .
             "  draw " . pairmap( 4, $cx[4]+$s, $cy[4]-$s )
              . " -- " . pairmap( 4, $cx[4]-$s, $cy[4]+$s )
              . ";\n";

#  Write the end of the metapost program.
      $mp .= "endfig;\n";
      $mp .= "end";

#  Convert the metapost code into postscript.
      my( $ps ) = makeps( $mp );
      print( $ps );

#  End.
      exit;


########################################################################
#  Utility subroutines.

#  Subroutine to turn a string containing metapost code into a string
#  containing postscript.
   sub makeps {
      my( $mp ) = shift;
      my( $tmpstub ) = "tmp";
      open( MP, ">$tmpstub.mp" );
      print( MP $mp );
      close( MP );
      system( "mpost $tmpstub.mp >/dev/null" );
      open( PS, "<$tmpstub.1" );
      my( $ps ) = join( '', <PS> );
      close( PS );
      return( $ps )
   }


# $Id$
