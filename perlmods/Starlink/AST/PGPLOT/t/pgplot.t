#!perl

use strict;
use Test::More tests => 16;

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
is( sprintf( "%.5", $x[ 0 ]), 
    sprintf( "%.5", 2.00000000000000), "X co-ord bottom left" );
is( sprintf( "%.5", $y[ 0 ]), 
    sprintf( "%.5", 3.76923082023859), "Y co-ord bottom left" );
is( sprintf( "%.5", $x[ 1 ]), 
    sprintf( "%.5", 2.03543463296149), "X co-ord bottom right" );
is( sprintf( "%.5", $y[ 1 ]), 
    sprintf( "%.5", 3.76923082023859), "Y co-ord bottom right" );
is( sprintf( "%.5", $x[ 2 ]), 
    sprintf( "%.5", 2.03543463296149), "X co-ord top right" );
is( sprintf( "%.5", $y[ 2 ]), 
    sprintf( "%.5", 4.12499997764826), "Y co-ord top right" );
is( sprintf( "%.5", $x[ 3 ]), 
    sprintf( "%.5", 2.00000000000000), "X co-ord top left" );
is( sprintf( "%.5", $y[ 3 ]), 
    sprintf( "%.5", 4.12499997764826), "Y co-ord top left" );

PGPLOT::pgline( $#x, \@x, \@y );

# _GFlush();
is( Starlink::AST::PGPLOT::_GFlush(), 1, "Calling _GFlush()" );




# Done!
exit;
