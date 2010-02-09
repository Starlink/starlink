#!perl

use strict;
use Test::More;

BEGIN {
 # pgbegin( $unit, $file, $nxsub, $nysub );
 eval { require PGPLOT; PGPLOT::pgbegin(0,"/xw",1,1) };
 if ( $@ ) {
   plan skip_all => "PGPLOT module not installed.";
   exit;
 } else {
   plan tests => 15;
 }  
 
};

require_ok("Starlink::AST");
require_ok("Starlink::AST::PGPLOT");

# pgenv( $xmin, $xmax, $ymin, $ymax, $axis_scaling, $axis_type );
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
my ( $status, $xb, $yb );
($status, $xb, $yb ) =
      Starlink::AST::PGPLOT::_GTxExt( "Testing", 2, 4, "CC", 0, 1 );
is( $status, 1, "Calling _GTxtEx()" );

$$xb[4] = $$xb[0]; $$yb[4] = $$yb[0];
Starlink::AST::PGPLOT::_GLine( $xb, $yb );


# _GText( $text, $x, $y, $justification, $upx, $upy );
is( Starlink::AST::PGPLOT::_GText( "Testing", 2, 6, "CC", 0.5, 0.5),
    1, "Calling _GText()" );

# _GTxtEx( $text, $x, $y, $justification, $upx, $upy, $xb, $yb );
($status, $xb, $yb ) =
      Starlink::AST::PGPLOT::_GTxExt( "Testing", 2, 6, "CC", 0.5, 0.5);
is( $status, 1, "Calling _GTxtEx()" );

$xb->[4] = $xb->[0]; $yb->[4] = $yb->[0];
Starlink::AST::PGPLOT::_GLine( $xb, $yb );

my $upx = 1.;
my $upy = -1;
Starlink::AST::PGPLOT::_GText( "Testing", 4, 6, "CL", $upx, $upy);
Starlink::AST::PGPLOT::_GMark([4],[6],24);

# _GQch()
my ( $chv, $chh );
( $status, $chv, $chh ) = Starlink::AST::PGPLOT::_GQch();
is( $status, 1, "Calling _GQch()" );

# These tests are unreliable at present since they vary depending
# on screen size
#is( $chv, 0.224459261126449, "Height of characters from vertical baseline" );
#is( $chh, 0.316722930654433, "Height of characters from horizontal baseline" );

# _GAttr
my $old_value;
( $status, $old_value ) = 
  Starlink::AST::PGPLOT::_GAttr( Starlink::AST::Grf::GRF__COLOUR(), 3, undef );

is( $status, 1, "Calling _GAttr()" );
is( $old_value, 1, "Checking old GRF__COLOUR value" );

( $status, $old_value ) = 
  Starlink::AST::PGPLOT::_GAttr( Starlink::AST::Grf::GRF__WIDTH(), 5, undef );

is( $status, 1, "Calling _GAttr()" );
# Unreliable depends on plot surface dimensions
#is( $old_value, 0.71376748335625, "Checking old GRF__WIDTH value" );


$x[0] = 5; $y[0] = 5;
$x[1] = 6; $y[1] = 6;
$x[2] = 7; $y[2] = 7;
$x[3] = 8; $y[3] = 8;

# _Gline( \@x, \@y );
is( Starlink::AST::PGPLOT::_GLine( \@x, \@y ), 1, "Calling _GLine()" );

# _GMark( \@x, \@y, $type );
is( Starlink::AST::PGPLOT::_GMark( \@x, \@y, 6 ), 1, "Calling _GMark()" );

# _GFlush();
is( Starlink::AST::PGPLOT::_GFlush(), 1, "Calling _GFlush()" );

# Done!
sleep(2);
exit;
