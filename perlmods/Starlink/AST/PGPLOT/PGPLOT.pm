package Starlink::AST::PGPLOT;

use strict;
use vars qw/ $VERSION /;
use constant R2D => 57.29578;  # Radians to degrees factor

use PGPLOT;

'$Revision$ ' =~ /.*:\s(.*)\s\$/ && ($VERSION = $1);

=head1 NAME

Starlink::AST::PGPLOT - AST wrapper to the PGPLOT library

=head1 SYNOPSIS

  use Starlink::AST::PGPLOT
 
=head1 DESCRIPTION
  
This file implements the low level graphics functions required by the rest
of AST, by calling suitable PGPLOT functions (the FORTRAN PGPLOT interface
is used).

This file can be used as a template for the development of similar modules
to support alternative graphics systems.

=head1 REVISION

$Id$

=head1 METHODS

=item B<GFlush>

This function ensures that the display device is up-to-date, by flushing 
any pending graphics to the output device.

   _GFlush();

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

   my $status = _GText( $text, $x, $y, $justification, $upx, $upy );

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
        unless( $just1 =~ /[TBC]/ ) {
           print "_GText: bad vertical justification defaulting to 'C'\n";
           $just1 = "C";
        }
        unless( $just2 =~ /[LCR]/ ) {
           print "_GText: bad horizontal justification defaulting to 'C'\n";
           $just2 = "C"; 
        }
      } else {
         print "_GText: No justification string defaulting to 'CC'\n";
         $just1 = "C";
         $just2 = "C";
      }
      $just = $just1 . $just2;
      
      # get the axis scaling
      my ( $ret, $alpha, $beta ) = _GAxScale();
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
         my ( $xbox, $ybox );
         pgqtxt( $x, $y, $angle, $fjust, $text, $xbox, $ybox );

         # Normalise the up-vector in world coordinates.
         my $uplen = sqrt( $upx*$upx + $upy*$upy );
         if( $uplen > 0.0 ){ 
            $upx /= $uplen;
            $upy /= $uplen;
         } else {
            print "_GText: Zero length up-vector supplied.";
            return 0;
         }      

         # Find the height of the text above the base-line. Note, the PGPLOT  
         # manual is not clear about the order of the corners returned by
         # pgqtxt, so we have to find the largest distance between the corners 
         # in the direction of the supplied up-vector. */
         my $hu = 0.0;
         for my $i ( 0 ... 4 ) {
            my $test = $upx*( $$xbox[$i] - $x ) + $upy*( $$ybox[$i] - $y );
            $hu = $test if $test > $hu;
         }

         # Adjust the vertical position of the reference point, since PGPLOT
         # requires it to be at the bottom of the text. */
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


=item B<_GAxScale>

This function returns two values (one for each axis) which scale
increments on the corresponding axis into a "normal" coordinate system in
which: The axes have equal scale in terms of (for instance) millimetres
per unit distance, X values increase from left to right and the Y values 
increase from bottom to top.

   my ( $status, $alpha, $beta ) = _GAxScale()

=cut

sub _GAxScale {
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
       print "_GAxScale: The graphics window has zero size\n";
       $ret = 0;
    }
    return ( $ret, $alpha, $beta );
}       

=head1 COPYRIGHT

Copyright (C) 2004 University of Exeter. All Rights Reserved.

This program is free software; you can redistribute it and/or modify 
it under the terms of the GNU Public License.

=head1 AUTHORS

Alasdair Allan E<lt>aa@astro.ex.ac.ukE<gt>,

=cut

1;
