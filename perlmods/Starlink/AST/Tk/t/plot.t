#!perl

use strict;
use Test::More tests => 7;
use Data::Dumper;
use Tk;

require_ok("Starlink::AST");
require_ok("Starlink::AST::Tk");

use File::Spec;
use Astro::FITS::CFITSIO;
use Astro::FITS::Header::CFITSIO;

Starlink::AST::Begin();

# FITS File
# ---------
my $file = File::Spec->catfile( "PGPLOT", "t", "m31.fit" );

# Get FITS Header
# ---------------

my $header = new Astro::FITS::Header::CFITSIO( File => $file );
my @cards = $header->cards();

my $nx = $header->value("NAXIS1");
my $ny = $header->value("NAXIS2");

# Make FitsChan
# -------------
my $wcsinfo = $header->get_wcs();
isa_ok( $wcsinfo, "Starlink::AST::FrameSet" );

# create Tk test harness
# ----------------------
my $c = create_window();

# Change FrameSet
# ---------------
$wcsinfo->Set( System => "GALACTIC" );

# AST axes
# --------
my $plot = Starlink::AST::Plot->new( $wcsinfo, 
   [0.1,0.1,0.9,0.9],[0.5,0.5, $nx+0.5, $ny+0.5], "Grid=1");
isa_ok( $plot, "Starlink::AST::Plot" );

my $status = $plot->tk( $c );
is( $status, 1, "Result from registering Tk with AST" );

$plot->Set( Colour => 2, Width => 5 );
$plot->Grid();

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
