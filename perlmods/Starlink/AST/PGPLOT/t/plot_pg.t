#!perl

use strict;
use Test::More;
use Data::Dumper;

require_ok( "Starlink::AST" );
require_ok( "Starlink::AST::PGPLOT" );

use File::Spec;

BEGIN {
 
 use PGPLOT;
 eval { PGPLOT::pgbegin(0,"/xw",1,1) };
 if ( $@ ) {
   plan skip_all => "PGPLOT module not installed.";
   exit;
 } 
 
 eval "use Astro::FITS::CFITSIO;";
 if ( $@ ) {
   plan skip_all => "Astro::FITS::CFITSIO not installed.";
   exit;
 }
 
 eval "use Astro::FITS::Header::CFITSIO;";
 if ( $@ ) {
   plan skip_all => "Astro::FITS::Header::CFITSIO not installed.";
   exit;
 }
 
 plan tests => 5;  
 
};

Starlink::AST::Begin();

# FITS File
# ---------
my $file = File::Spec->catfile( File::Spec->updir(), "data", "m31.fit" );

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
my $nx = $header->value("NAXIS1");
my $ny = $header->value("NAXIS2");
pgpage();
pgwnad( 0,1,0,1 );

my ( $x1, $x2, $y1, $y2 ) = (0,1,0,1);

 
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
#$wcsinfo->Set( System => "GALACTIC" );

# AST axes
# --------
my $plot = Starlink::AST::Plot->new( $wcsinfo, 
   [$xleft,$ybottom,$xright,$ytop],[0.5,0.5, $nx+0.5, $ny+0.5], "Grid=1");
isa_ok( $plot, "Starlink::AST::Plot" );

my $status = $plot->pgplot();
is( $status, 1, "Result from registering PGPLOT with AST" );

#$plot->Set( Colour => 2, Width => 5 );
$plot->Grid();

$plot->Set( "Current=1" );
$plot->Text("Test Text 1", [0.4,0.4],[0.0,1.0],"CC");
$plot->Text("Test Text 2", [0.5,0.5],[0.0,1.0],"CC");
$plot->Text("Test Text 2", [0.6,0.6],[0.0,1.0],"CC");
# Done!
sleep(2);
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
