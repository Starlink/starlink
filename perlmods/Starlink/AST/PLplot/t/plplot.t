#!perl

use strict;
use Test::More;

BEGIN {
 eval { use Graphics::PLPLOT; };
 if ( $@ ) {
   plan skip_all => "PLPLOT module not installed.";
   exit;
 } else {
   plan tests => 1;
 }  
 
};

print "# PLplot Version: ". &plgver() ."\n";

plsdev( "xwin" );
plinit();
pladv(0);

plvpor(0, 1, 0, 1);
plwind(0, 1, 0, 1);

my ( @x, @y );
$x[0] = 1; $y[0] = 1;
$x[1] = 2; $y[1] = 2;
$x[2] = 3; $y[2] = 3;
$x[3] = 4; $y[3] = 4;

# _Gline( \@x, \@y );
is( Starlink::AST::PGPLOT::_GLine( \@x, \@y ), 1, "Calling _GLine()" );

# _GMark( \@x, \@y, $type );
is( Starlink::AST::PGPLOT::_GMark( \@x, \@y, 6 ), 1, "Calling _GMark()" );

# _GFlush();
is( Starlink::AST::PGPLOT::_GFlush(), 1, "Calling _GFlush()" );

plend();

# Done!
sleep(1);
exit;
