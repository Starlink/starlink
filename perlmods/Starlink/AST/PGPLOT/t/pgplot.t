#!perl

use strict;
use Test::More tests => 8;

require_ok("PGPLOT");
require_ok("Starlink::AST::PGPLOT");

# pgbegin( $unit, $file, $nxsub, $nysub );
# pgenv( $xmin, $xmax, $ymin, $ymax, $axis_scaling, $axis_type );
is( PGPLOT::pgbegin(0,"/xserve",1,1), 1, "Calling PGBEGIN()" );
PGPLOT::pgenv(0,10,0,10,0,0);

my ( @x, @y );
$x[0] = 1; $y[0] = 1;
$x[1] = 2; $y[1] = 2;
$x[2] = 3; $y[2] = 3;
$x[3] = 4; $y[3] = 4;

# _Gline( \@x, \@y );
is( Starlink::AST::PGPLOT::_GLine( \@x, \@y ), 1, "Calling _GLine()" );

# _GMark( \@x, \@y, $type );
is( Starlink::AST::PGPLOT::_GMark( \@x, \@y, 6 ), 1, "Calling _GMark()" );

# _GText( $text, $x, $y, $justification, $upx, $upy );
is( Starlink::AST::PGPLOT::_GText( "Testing", 2, 4, "CC", 0, 1), 
    1, "Calling _GText()" );

# _GTxtEx( $text, $x, $y, $justification, $upx, $upy, $xb, $yb );
is( Starlink::AST::PGPLOT::_GTxtEx( "Testing", 2, 4, "CC", 0, 1, \@x, \@y), 
    1, "Calling _GTxtEx()" );

$x[4] = $x[0]; $y[4] = $y[0];
PGPLOT::pgline( $#x+1, \@x, \@y );

# _GFlush();
is( Starlink::AST::PGPLOT::_GFlush(), 1, "Calling _GFlush()" );




# Done!
exit;
