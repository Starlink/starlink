#!perl

use strict;
use Test::More tests => 5;
use Data::Dumper;
use Tk;
use Tk::JPEG;

require_ok("Starlink::AST");
require_ok("Starlink::AST::Tk");

use File::Spec;
use Astro::FITS::CFITSIO;
use Astro::FITS::Header::CFITSIO;

Starlink::AST::Begin();

# FITS File
# ---------
my $file = File::Spec->catfile( File::Spec->updir(), "data", "m31.fit" );
my $jpeg = File::Spec->catfile( File::Spec->updir(), "data", "m31.jpg" );

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

# Read data 
# ---------
my $x1 = 0.1*$c->cget( '-width' );
my $y1 = 0.9*$c->cget( '-height' );
my $x2 = 0.9*$c->cget( '-width' );
my $y2 = 0.1*$c->cget( '-height' );

my $xzoom = ($x2-$x1)/300;
my $yzoom = ($y1-$y2)/300;
#print "# $xzoom, $yzoom";
my $jpg = $c->Photo( -format => 'jpeg', -file => $jpeg );

#my $zoom = $c->Photo();
#$zoom->copy( $jpg, -zoom => (2,2) );
#1.70666666666667, 1.28
 
# Plot image
# ---------

$c->createImage( $x1, $y1, -image => $jpg, -anchor => 'sw' );

# Change FrameSet
# ---------------
#$wcsinfo->Set( System => "GALACTIC" );

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
print "# Entering MainLoop()\n";
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
   $MW->after( 3000, sub { exit; } );

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

# read FITS file
sub read_file {
   my $file = shift;

   my $status = 0;
   my $fptr = Astro::FITS::CFITSIO::open_file(
             $file, Astro::FITS::CFITSIO::READONLY(), $status);

   my ($array, $nullarray, $anynull);
   $fptr->read_pixnull( 
     Astro::FITS::CFITSIO::TLONG(), [1,1], $nx*$ny, $array, $nullarray, 
     $anynull ,$status);
   $fptr->close_file($status);

   return $array;
}

1;   
