#!perl

use strict;
use Test::More;

BEGIN {
 eval {require Graphics::PLplot; };
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
draw_and_plot( "Testing1", 0.2, 0.4, "CC", 0, 1 );
draw_and_plot( "Testing2", 0.2, 0.6, "BR", 0.5, 0.5);

# PGPLOT definitions of upx/upy:
#   upx  upy   orientation
#    0    0     no text
#    1    0     vertical, facing left
#    1    1     45 degrees facing down left
#   -1    1     45 degrees facing down right
#   -1    0     vertical facing right
#    0   -1     horizontal facing up
#    0    1     horizontal facing down
#    1   -1     45 degrees facing up left

my $upxx = 4;
my $upyy =  3;
Graphics::PLplot::plptex( 0.4,0.6, $upxx, $upyy, 0.7,"$upxx,$upyy,j=0.7");
Graphics::PLplot::plcol0(2);
Graphics::PLplot::plpoin( [0.4],[0.6],5);
print "# Angle = ". (atan2($upyy, $upxx) * 180/ 3.141592654)."\n";

# Make a bit bigger
plschr(3.5,3);

draw_and_plot ("ASTText", 0.6, 0.75, "CC", 0, 1);
draw_and_plot ("Down Right", 0.7, 0.4, "CC", -3, 4);


# _GFlush();
is( Starlink::AST::PLplot::_GFlush(), 1, "Calling _GFlush()" );

# Done!
sleep(1);
plspause(0);

plend();

exit;


sub draw_and_plot {
  Starlink::AST::PLplot::_GText( @_ );
  my ($status, $xb, $yb ) =
    Starlink::AST::PLplot::_GTxExt( @_ );
  is($status, 1, "Status check from GTxExt");
  $xb->[4] = $xb->[0]; $yb->[4] = $yb->[0];
  Starlink::AST::PLplot::_GLine( $xb, $yb );
  Starlink::AST::PLplot::_GMark([$_[1]],[$_[2]],5);
}
