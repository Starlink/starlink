#!perl

use strict;
use Test::More tests => 6;
use Data::Dumper;

require_ok( "Starlink::AST" );
require_ok( "Starlink::AST::PGPLOT" );

use PGPLOT;
use File::Spec;
use Astro::FITS::CFITSIO;
use Astro::FITS::Header::CFITSIO;

Starlink::AST::Begin();

# FITS File
# ---------
#my $file = File::Spec->catfile( "PGPLOT", "t", "m31.fit" );
my $file = File::Spec->catfile( "t", "m31.fit" );

# Get FITS Header
# ---------------

my $header = new Astro::FITS::Header::CFITSIO( File => $file );
my @cards = $header->cards();

# Make FitsChan
# -------------
my $wcsinfo = $header->get_wcs();
isa_ok( $wcsinfo, "Starlink::AST::FrameSet" );

# Set up window
# -------------

is( PGPLOT::pgbegin(0,"/xserve",1,1), 1, "Result from the PGBEGIN() call" );

my $nx = $header->value("NAXIS1");
my $ny = $header->value("NAXIS2");
pgpage();
pgwnad( 0.1, 0.9, 0.1, 0.9 );

my ( $x1, $x2, $y1, $y2 );
pgqwin( $x1, $x2, $y1, $y2 );
 
my $xscale = ( $x2 - $x1 ) / $nx;
my $yscale = ( $y2 - $y1 ) / $ny;
my $scale = ( $xscale < $yscale ) ? $xscale : $yscale;
my $xleft   = 0.5 * ( $x1 + $x2 - $nx * $scale );
my $xright  = 0.5 * ( $x1 + $x2 + $nx * $scale );
my $ybottom = 0.5 * ( $y1 + $y2 - $ny * $scale );
my $ytop    = 0.5 * ( $y1 + $y2 + $ny * $scale );

# Read data 
# ---------
my $array = read_file( $file );
         
pggray( $array, $nx, $ny, 1, $nx, 1, $ny, 10000, 0, 
  [ $xleft-0.5*$scale, $scale, 0.0, $ybottom-0.5*$scale, 0.0, $scale ] );

# Change FrameSet
# ---------------
$wcsinfo->Set( System => "GALACTIC" );

# AST axes
# --------
my $plot = Starlink::AST::Plot->new( $wcsinfo, 
   [$xleft,$ybottom,$xright,$ytop],[0.5,0.5, $nx+0.5, $ny+0.5], "Grid=1");
isa_ok( $plot, "Starlink::AST::Plot" );

my $status = $plot->pgplot();
is( $status, 1, "Result from registering PGPLOT with AST" );

#$plot->Set( Colour => 2, Width => 5 );
$plot->Grid();

# Done!
exit;

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
