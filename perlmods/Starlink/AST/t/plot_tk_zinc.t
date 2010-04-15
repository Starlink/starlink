#!perl

use strict;
use Test::More;
use Data::Dumper;
use File::Spec;

BEGIN {

 eval "use Tk";
 if ($@) {
   plan skip_all => "Tk module not installed";
   exit;
 }

 eval "use Tk::Zinc";
 if ($@) {
   plan skip_all => "Tk::Zinc module not installed";
   exit;
 }

 eval "use Tk::Button";
 if ( $@ ) {
   plan skip_all => "Tk modules not installed";
   exit;
 }

 eval "use Tk::JPEG;";
 if ( $@ ) {
   plan skip_all => "Tk::JPEG modules not installed";
   exit;
 }

 eval "use Astro::FITS::Header;";
 if ( $@ ) {
   plan skip_all => "Astro::FITS::Header not installed.";
   exit;
 }

 plan tests => 5;

};

require_ok("Starlink::AST");
require_ok("Starlink::AST::Tk");

my $zoom = 1;
my @factor;
$factor[0] = 1.7;
$factor[1] = 1.7;
print "# zoom = $zoom, xfactor = $factor[0], yfactor = $factor[1]\n";

Starlink::AST::Begin();

# Get FITS Header
# ---------------
my @cards;
while(<DATA>) {
  push @cards, $_;
}
my $header = new Astro::FITS::Header( Cards => \@cards );
my @axes;
$axes[0] = $header->value( "NAXIS1" );
$axes[1] = $header->value( "NAXIS2" );

# Make FitsChan
# -------------
my $wcsinfo;
if ($header->can("get_wcs")) {
  $wcsinfo = $header->get_wcs();
} else {
  # Use fallback position
  $wcsinfo = get_wcs( $header );
}
isa_ok( $wcsinfo, "Starlink::AST::FrameSet" );

# Create Tk test harness
# ----------------------
my $c = create_window( \@axes, $zoom, \@factor );

# Handle data
# -----------
my $width = $c->cget( '-width' );
my $height = $c->cget( '-height' );
print "# width = $width, height = $height\n";

my $xmin = $width/2 - $axes[0]*$zoom/2;
my $ymin = $height/2 + $axes[1]*$zoom/2;
print "# xmin = $xmin, ymin = $ymin\n";

# Plot image
# ---------
my $jpeg = File::Spec->catfile( "data", "m31.jpg" );
my $jpg = $c->Photo( -format => 'jpeg', -file => $jpeg );

my $image = $c->Photo();
$image->copy($jpg, -zoom => ($zoom, $zoom));
$c->add( 'icon', 1, -position => [$xmin, $ymin], -image => $image, -anchor => 'sw',
	 -tags => [ 'image' ] );

# Handle data
# -----------
my ( $x1, $y2, $x2, $y1 ) = $c->bbox( "image" );
print "# x1 = $x1, x2 = $x2,y1 = $y1, y2 = $y2\n";

my $xleft   = $x1/$width;
my $xright  = ($x1 + $axes[0]*$zoom)/$width;
my $ybottom = $y2/$height;
my $ytop    = ($y2 + $axes[1]*$zoom)/$height;
print "# xleft = $xleft, xright = $xright\n";
print "# ytop = $ytop, ybottom = $ybottom\n";

# Change Frame
# ------------
#$wcsinfo->Set( System => "GALACTIC" );

# AST axes
# --------
my $plot = Starlink::AST::Plot->new( $wcsinfo,
   [$xleft, $ybottom, $xright, $ytop],
   [0,0, $axes[0], $axes[1]], 'Grid=1, Title="M31 Test Image"');
isa_ok( $plot, "Starlink::AST::Plot" );

my $status = $plot->tk( $c );
is( $status, 1, "Result from registering Tk with AST" );

$plot->Set( Colour => 2, Width => 5 );
$plot->Grid();

# Switch to GRAPHICS frame for easy plotting

my $ra1 = $wcsinfo->Unformat( 1, "0:40:00" );
my $dec1 = $wcsinfo->Unformat( 2, "41:30:00" );
my $ra2 = $wcsinfo->Unformat( 1, "0:44:00" );
my $dec2 = $wcsinfo->Unformat( 2, "42:00:00" );

print "\n# Current Frame " . $plot->Get( "Domain" ) . "\n";
print "# Plotting at $ra1, $dec1\n";
print "# Plotting at $ra2, $dec2\n";
$plot->Mark( 24, [$ra1, $ra2], [$dec1, $dec2] );

$plot->Set( Current => 1 );
print "\n# Current Frame " . $plot->Get( "Domain" ) . "\n";
$plot->Text("Test Text 1", [0.4,0.4],[-0.5,0.866],"CC");
$plot->Set( Colour => 3  );
$plot->Text("Test Text 2", [0.5,0.5],[0.0,-1.0],"CC");
$plot->Set( Colour => 4 );
$plot->Text("Test Text 3", [0.6,0.6],[0.5,0.866],"CC");

#$plot->Set( Colour => 6, Width => 5 );
$plot->Mark( 24, [0.6,0.5,0.4], [0.4, 0.3,0.3]  );

#$plot->Set( Colour => 2, Width => 5 );
#$plot->PolyCurve( [0.2,0.3,0.25], [0.8,0.5,0.5]);

# Tk MainLoop
# -----------
print "# Entering MainLoop()\n";
MainLoop();

# Done!
exit;

# A S S O C I A T E D   S U B - R O U T I N E S #############################

# test harness window
sub create_window {
   my $axes = shift;
   my $zoom = shift;
   my $factor = shift;

   my $MW = MainWindow->new();
   $MW->positionfrom("user");
   $MW->geometry("+40+100");
   $MW->title("Starlink::AST::Tk");
   $MW->iconname("Starlink::AST::Tk");
   $MW->configure( -cursor => "tcross" );
   $MW->after( 3000, sub { exit; } );

   # create the canvas widget
   my $canvas = $MW->Zinc( -width       => $axes[0]*$zoom*$$factor[0],
			   -height      => $axes[1]*$zoom*$$factor[1],
			   -backcolor  => 'darkgrey',
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

# Implementation of the get_wcs method for old versions of Astro::FITS::Header

sub get_wcs {
  my $self = shift;
  my $fchan = Starlink::AST::FitsChan->new();
  for my $i ( $self->cards() ) {
    $fchan->PutFits( $i, 0);
  }
  $fchan->Clear( "Card" );
  return $fchan->Read();
}

# read FITS file
#sub read_file_native {
#   my $file = shift;
#   my $axes = shift;
#
#   my $status = 0;
#   my $fptr = Astro::FITS::CFITSIO::open_file(
#             $file, Astro::FITS::CFITSIO::READONLY(), $status);
#
#   my ($array, $nullarray, $anynull);
#   $fptr->read_pixnull(
#       Astro::FITS::CFITSIO::TLONG(), [1,1], $$axes[0]*$$axes[1],
#       $array, $nullarray, $anynull ,$status );
#   $fptr->close_file($status);
#
#   return $array;
#}

#sub read_file_pdl {
#   my $file = shift;
#
#   my $image = PDL->rfits( $file );
#   return $image;
#}

__DATA__
SIMPLE  =                    T / file does conform to FITS standard
BITPIX  =                  -32 / number of bits per data pixel
NAXIS   =                    2 / number of data axes
NAXIS1  =                  300 / length of data axis 1
NAXIS2  =                  300 / length of data axis 2
EXTEND  =                    T / FITS dataset may contain extensions
COMMENT   FITS (Flexible Image Transport System) format is defined in 'Astronomy
COMMENT   and Astrophysics', volume 376, page 359; bibcode: 2001A&A...376..359H
OBJECT  = 'M31 (Digitised Sky Survey)'/ Title of the dataset
DATE    = '2004-02-25T09:33:25'/ file creation date (YYYY-MM-DDThh:mm:ss UT)
ORIGIN  = 'CASB -- STScI'      / Origin of FITS image
BSCALE  =                  1.0 / True_value = BSCALE * FITS_value + BZERO
BZERO   =                  0.0 / True_value = BSCALE * FITS_value + BZERO
HDUCLAS1= 'NDF     '           / Starlink NDF (hierarchical n-dim format)
HDUCLAS2= 'DATA    '           / Array component subclass
CTYPE1  = 'RA---TAN'           / X-axis type
CTYPE2  = 'DEC--TAN'           / Y-axis type
CRVAL1  =              10.6847 / Reference pixel value
CRVAL2  =               41.269 / Reference pixel value
CRPIX1  =                150.5 / Reference pixel
CRPIX2  =                150.5 / Reference pixel
CDELT1  =                -0.01 / Degrees/pixel
CDELT2  =                 0.01 / Degrees/pixel
CROTA2  =                  0.0 / Axis rotation
EPOCH   =               2000.0 / Epoch of reference equinox
COMMENT
COMMENT This file was produced by the SkyView survey analysis system from
COMMENT available astronomical surveys.  The data are formatted
COMMENT as a simple two-dimensional FITS image with the same units as
COMMENT the orginal survey.  A single ASCII table extension may be present
COMMENT which describes catalog objects found within the field of view.         COMMENT Copies of relevant copyright notices are included in this file.
COMMENT
COMMENT Questions should be directed to:
COMMENT
COMMENT     scollick@skyview.gsfc.nasa.gov
COMMENT          or
COMMENT     mcglynn@grossc.gsfc.nasa.gov
COMMENT
COMMENT     SkyView
COMMENT     Code 668.1
COMMENT     Goddard Space Flight Center, Greenbelt, MD 20771
COMMENT     301-286-7780
COMMENT
COMMENT SkyView is supported by NASA ADP grant NAS 5-32068.
COMMENT
SURVEY  = 'Digitized Sky Survey'
BANDPASS=                    8 / GSSS Bandpass code
TELESCOP= 'Palomar 48-inch Schmidt'/ Telescope where plate taken
SITELONG= '-116:51:48.00'      / Longitude of Observatory
SITELAT = '+33:24:24.00'       / Latitute of Observatory
SCANIMG = 'CASB -- STScI     ' / Name of original scan
COMMENT  Based on photographic data obtained using Oschin Schmidt Telescope
COMMENT  on Palomar Mountain.  The Palomar Observatory Sky Survey was funded
COMMENT  by the National Geographic Society.  The Oschin Shmidt Telescope is
COMMENT  operated by the California Institue of Technology and Palomar
COMMENT  Observatory.  The plates were processed into the present compressed
COMMENT  digital format with their permission.  The Digitized Sky Survey was
COMMENT  produced at the Space Telescope Science Institute (ST ScI) under
COMMENT  U. S. Goverment grant NAG W-2166.
COMMENT
COMMENT  Investigators using these scans are requested to include the above
COMMENT  acknowledgements in any publications.
COMMENT
COMMENT  Copyright (c) 1994, Association of Universities for Research in
COMMENT  Astronomy, Inc.  All rights reserved.
COMMENT
COMMENT  Properties of original survey:
COMMENT  Provenance - Data taken by Oshcin Schmidt Telescope on Palomar Mountain
COMMENT    Compressed and distribution by Space Telescope Science Institute.
COMMENT  Copyright - AURA Inc. based upon photographic data obtained using
COMMENT    Oschin Schmidt Telescope, restrictions on data transmissions
COMMENT    prior to April, 1996.
COMMENT  Frequency- 600 THz (J band image)
COMMENT  Pixel Scale - 1.7".
COMMENT  Pixel Units - Pixel values are given as scaled densities.
COMMENT  Resolution - Depends on plate. Typically 2" or better.
COMMENT  Coordinate system - Equatorial
COMMENT  Projection - Schmidt
COMMENT  Equinox - 2000
COMMENT  Epoch - ca. 1957
COMMENT
COMMENT  Note that images generated by SkyView may contain distortions
COMMENT  due to resampling of the data.
END
