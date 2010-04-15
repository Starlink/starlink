#!perl

use strict;
use Test::More;

BEGIN {

 eval "use Tk; use Tk::Button;";
 if ( $@ ) {
   plan skip_all => "Tk modules not installed";
   exit;
 }

 eval "use Tk::Zinc";
 if ($@) {
   plan skip_all => "Tk::Zinc module not installed";
   exit;
 }

 # we can test
 plan tests => 18;
};

require_ok("Starlink::AST");
require_ok("Starlink::AST::Tk");

# create Tk test harness
my $c = create_window();

# create some test data
my ( @x, @y );
$x[0] = 0.1; $y[0] = 0.1;
$x[1] = 0.2; $y[1] = 0.2;
$x[2] = 0.3; $y[2] = 0.3;
$x[3] = 0.4; $y[3] = 0.4;

# create GExternal array
my $attrs = {};
my $e = [ $c, 0.0, 1.0, 0.0, 1.0, 0.0, 1.0, 0.0, 1.0, $attrs ];
Starlink::AST::Tk::_init_canvas_attrs( $c, $attrs);

# _GAttr
my ( $status, $old_value ) =
  Starlink::AST::Tk::_GAttr( $e, Starlink::AST::Grf::GRF__COLOUR(), 3,
                             Starlink::AST::Grf::GRF__MARK() );

is( $status, 1, "Calling _GAttr()" );
is($old_value, Starlink::AST::AST__BAD(),"Checking default GRF__COLOUR value");

# check value we just sent
( $status, $old_value ) =
  Starlink::AST::Tk::_GAttr( $e, Starlink::AST::Grf::GRF__COLOUR(),
			     Starlink::AST::AST__BAD(),
                             Starlink::AST::Grf::GRF__MARK() );
is($old_value, 3, "Checking previous GRF__COLOUR for GRF__MARK");

# set a coloured line
( $status, $old_value ) =
  Starlink::AST::Tk::_GAttr( $e, Starlink::AST::Grf::GRF__COLOUR(),
			     5,
                             Starlink::AST::Grf::GRF__LINE() );

# _GFlush( $c );
is( Starlink::AST::Tk::_GFlush( $e ), 1, "Calling _GFlush()" );

# _Gline( \@x, \@y );
is( Starlink::AST::Tk::_GLine( $e, \@x, \@y ), 1, "Calling _GLine()" );

# _GMark( \@x, \@y, $type );
( $status, $old_value ) =
  Starlink::AST::Tk::_GAttr( $e, Starlink::AST::Grf::GRF__SIZE(),
			     1.5,
                             Starlink::AST::Grf::GRF__MARK() );
is( Starlink::AST::Tk::_GMark( $e, \@x, \@y, 17 ), 1, "Calling _GMark()" );

_text_and_box( $e, "Testing", 0.2, 0.4, "CL", 0, 1);
( $status, $old_value ) =
  Starlink::AST::Tk::_GAttr( $e, Starlink::AST::Grf::GRF__COLOUR(),
			     6,
                             Starlink::AST::Grf::GRF__TEXT() );
_text_and_box( $e, "Testing", 0.2, 0.6, "CC", 1, 0);
_text_and_box( $e, "TestingCC", 0.4, 0.6, "CC", -1, 0);
_text_and_box( $e, "TestingBL", 0.4, 0.6, "BL", -1, 0);

( $status, $old_value) = Starlink::AST::Tk::_GAttr( $e, Starlink::AST::Grf::GRF__SIZE(),
						    2,
						    Starlink::AST::Grf::GRF__TEXT() );
_text_and_box( $e, "Size", 0.6, 0.3, "CC", 1, 0);


# enter Tk mainloop()
MainLoop();

# Done!
exit;

# A S S O C I A T E D   S U B - R O U T I N E S #############################

# test harness window
sub create_window {

   my $MW = MainWindow->new();
   $MW->positionfrom("user");
   $MW->geometry("+40+100");
   $MW->title("Starlink::AST::Tk");
   $MW->iconname("Starlink::AST::Tk");
   $MW->configure( -cursor => "tcross" );
   $MW->after( 1000, sub { exit; } );

   # create the canvas widget
   my $canvas = $MW->Zinc( -width       => 640,
			   -height      => 480,
			   -backcolor   => 'darkgrey',
			   -borderwidth => 3 );
   $canvas->pack();

   my $frame = $MW->Frame( -relief => 'flat', -borderwidth => 1 );
   $frame->pack( -side => 'bottom', -fill => 'both', -expand => 'yes');

   my $button = $frame->Button( -text             => 'Quit',
                                -font             => 'Helvetica 12',
	   		        -activeforeground => 'white',
                                -activebackground => 'red',
                                -foreground       => 'white',
                                -background       => 'darkgrey',
                                -borderwidth      => 3,
                                -command => sub { exit; } );
   $button->pack( -side => 'right' );

   return $canvas;
}

sub _text_and_box {
  my ( $e, $text, $xpos, $ypos, $just, $upx, $upy ) = @_;

  is( Starlink::AST::Tk::_GText( $e, $text, $xpos, $ypos, $just, $upx, $upy),
      1, "Calling _GText()" );

  # _GTxExt( $text, $x, $y, $justification, $upx, $upy, $xb, $yb );
  my ($status, $xb, $yb ) =
    Starlink::AST::Tk::_GTxExt( $e, $text, $xpos, $ypos, $just, $upx, $upy );
  is( $status, 1, "Calling _GTxtEx()" );

  $$xb[4] = $$xb[0]; $$yb[4] = $$yb[0];
  Starlink::AST::Tk::_GLine( $e, $xb, $yb );

}


1;
