#!perl

use strict;
use Test::More tests => 7;
use Tk;

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
my $e = [ $c, 0.0, 1.0, 0.0, 1.0 ];

# _Gline( \@x, \@y );
is( Starlink::AST::Tk::_GLine( $e, \@x, \@y ), 1, "Calling _GLine()" );

# _GMark( \@x, \@y, $type );
is( Starlink::AST::Tk::_GMark( $e, \@x, \@y, 20 ), 1, "Calling _GMark()" );


# _GAttrb
my ( $status, $old_value ) = 
  Starlink::AST::Tk::_GAttr( $e, Starlink::AST::Grf::GRF__COLOUR(), 3, 
                             Starlink::AST::Grf::GRF__MARK() );

is( $status, 1, "Calling _GAttr()" );
is( $old_value, Starlink::AST::AST__BAD(), "Checking old GRF__COLOUR value" );

# _GFlush( $c );
is( Starlink::AST::Tk::_GFlush( $e ), 1, "Calling _GFlush()" );

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
   my $canvas = $MW->Canvas( -width       => 640, 
                             -height      => 480, 
                             -background  => 'dark grey',
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

1;   
