#!perl

use strict;
use Test::More;

BEGIN {
 eval { use Graphics::PLplot; };
 if ( $@ ) {
   plan skip_all => "Graphics::PLplot module not installed.";
   exit;
 } else {
   plan tests => 9;
 }  
 Graphics::PLplot->import(qw/ :all /);
 
};


require_ok("Starlink::AST");
require_ok("Starlink::AST::PLplot");

plsdev( "xwin" );
plinit();
pladv(0);

plvpor(0, 1, 0, 1);
plwind(0, 1, 0, 1);

plcol0(2);
plenv(0, 1, 0, 1, 0, 1);

my ( @x, @y );
$x[0] = 0.1; $y[0] = 0.1;
$x[1] = 0.2; $y[1] = 0.2;
$x[2] = 0.3; $y[2] = 0.3;
$x[3] = 0.4; $y[3] = 0.4;

# _Gline( \@x, \@y );
plcol0(1);
is( Starlink::AST::PLplot::_GLine( \@x, \@y ), 1, "Calling _GLine()" );

# _GMark( \@x, \@y, $type );
is( Starlink::AST::PLplot::_GMark( \@x, \@y, 6 ), 1, "Calling _GMark()" );

# _GText( $text, $x, $y, $justification, $upx, $upy );
is( Starlink::AST::PLplot::_GText( "Testing", 0.2, 0.4, "CC", 0, 1),
    1, "Calling _GText()" );

# _GTxtEx( $text, $x, $y, $justification, $upx, $upy, $xb, $yb );
my ( $status, $xb, $yb );
($status, $xb, $yb ) =
      Starlink::AST::PLplot::_GTxExt( "Testing", 0.2, 0.4, "CC", 0, 1 );
is( $status, 1, "Calling _GTxtEx()" );

$xb->[4] = $xb->[0]; $yb->[4] = $yb->[0];
Starlink::AST::PLplot::_GLine( $xb, $yb );


# _GText( $text, $x, $y, $justification, $upx, $upy );
is( Starlink::AST::PLplot::_GText( "Testing", 0.2, 0.6, "CC", 0.5, 0.5),
    1, "Calling _GText()" );

# _GTxtEx( $text, $x, $y, $justification, $upx, $upy, $xb, $yb );
my ( $status, $xb, $yb );
($status, $xb, $yb ) =
      Starlink::AST::PLplot::_GTxExt( "Testing", 0.2, 0.6, "CC", 0.5, 0.5);
is( $status, 1, "Calling _GTxtEx()" );

$xb->[4] = $xb->[0]; $yb->[4] = $yb->[0];
Starlink::AST::PLplot::_GLine( $xb, $yb );



# _GFlush();
is( Starlink::AST::PLplot::_GFlush(), 1, "Calling _GFlush()" );

plend();

# Done!
sleep(1);
exit;
