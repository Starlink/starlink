#!/usr/bin/perl -w


   my( $scale ) = 0.05;

   my( $da ) = 3.0;    # angular eccentricity
   my( $dp ) = 3.0;    # positional eccentricity
   my( $bo ) = 1.0;    # border size
   my( $dd ) = 3.0;    # nonlinear distortion

   

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
   
   my( @cx, @cy );
   ( $cx[4], $cy[4] ) = ( 1778, 3029 );
   ( $cx[1], $cy[1] ) = ( $cx[4] - 2048 - 50 * $bo - $dp * 16.74, 
                          $cy[4] + 0 + $dp * 10.14 );
   ( $cx[2], $cy[2] ) = ( 6144 - $cy[4] + 50 * $bo + $dp * 15.68, 
                          - 50 * $bo + $dp * 14.74 + $cx[4] - $dp * 13.07 );
   ( $cx[3], $cy[3] ) = ( $cx[4] + 2048 + 50 * $bo - $dp * 0.27,  
                          $cy[4] + 0 - $dp * 32.07 );

   my( $disco ) = -5.2e-10 * $dd;

   my( $mp ) = "";

   $mp .= q{
      prologues := 1;
      beginfig(1);
   };

   my( @points ) = (
      [ 0, 0 ], 
      [ 2048, 0 ],
      [ 2048, 4096 ],
      [ 0, 4096 ],
   );

   my( $px, $py );
   foreach $i ( 1 .. 4 ) {
      $mp .= "draw \n";
      foreach $p ( @points ) {
         my( $px ) = $p->[ 0 ];
         my( $py ) = $p->[ 1 ];
         my( $sx ) = $px - $cx[ $i ];
         my( $sy ) = $py - $cy[ $i ];
         my( $rx ) = $sx * cos( $dtheta[ $i ] ) - $sy * sin( $dtheta[ $i ] );
         my( $ry ) = $sx * sin( $dtheta[ $i ] ) + $sy * cos( $dtheta[ $i ] );
         my( $fx ) = $rx * ( 1 + $disco * ( $rx * $rx + $ry * $ry ) );
         my( $fy ) = $ry * ( 1 + $disco * ( $rx * $rx + $ry * $ry ) );
         $mp .= sprintf( "   (%.3f, %.3f) -- \n", $fx * $scale, $fy * $scale );
      }
      $mp .= "   cycle;\n";
    }

    $mp .= q{
       endfig;
       end
    };

    # print( $mp );
    my( $ps ) = makeps( $mp );
    print( $ps );
    exit;


#  Process metapost.
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
