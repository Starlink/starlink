#!perl

use strict;
use Test::More tests => 12;

require_ok("Starlink::AST");

Starlink::AST::Begin();

my @cards;
my $fchan = new Starlink::AST::FitsChan( );
while (<DATA>) {
  $fchan->PutFits($_ ,0);
}
$fchan->Clear( "Card" );

# FitsChan
# --------

# encoding type
is( $fchan->Get( "Encoding" ), "FITS-WCS",
    "Encoding type of the FitsChan object" );

# number of cards in the FitsChan
is ( $fchan->Get( "Ncard" ), 38, "Number of cards in FitsChan" );

# Get FrameSet
my $wcsinfo = $fchan->Read();
isa_ok( $wcsinfo, "Starlink::AST::FrameSet" );

# Mappings
# --------

# define some arrays (and references) to hold out inital coordinates
my ( @x, @y );
my $xpixel = \@x;
my $ypixel = \@y;

# FORWARD MAPPING
# ---------------
$x[0] = 0;
$y[0] = 0;;
$x[1] = 114;
$y[1] = 128;

my ( $xworld, $yworld) = $wcsinfo->Tran2( $xpixel, $ypixel, 1 );
is( $$xworld[0], 4.5, "Forward mapping of lower bound X co-ordinate" );
is( $$yworld[0], -0.5, "Forward mapping of lower bound Y co-ordinate" );
is( $$xworld[1], 118.5, "Forward mapping of upper bound X co-ordinate" );
is( $$yworld[1], 127.5, "Forward mapping of upper bound Y co-ordinate" );

# REVERSE MAPPING
# ---------------
$x[0] = 4.5;
$y[0] = -0.5;
$x[1] = 118.5;
$y[1] = 127.5;
($xworld, $yworld) = $wcsinfo->Tran2( $xpixel, $ypixel, 0 );
is( $$xworld[0], 0, "Reverse mapping of lower bound X co-ordinate" );
is( $$yworld[0], 0, "Reverse mapping of lower bound Y co-ordinate" );
is( $$xworld[1], 114, "Reverse mapping of upper bound X co-ordinate" );
is( $$yworld[1], 128, "Reverse mapping of upper bound Y co-ordinate" );

# Done!
exit;

__DATA__
SIMPLE  =                    T / file does conform to FITS standard
BITPIX  =                  -32 / number of bits per data pixel
NAXIS   =                    2 / number of data axes
NAXIS1  =                  114 / length of data axis 1
NAXIS2  =                  128 / length of data axis 2
CRPIX1  =                 57.0 / Reference pixel on axis 1
CRPIX2  =                 64.0 / Reference pixel on axis 2
CRVAL1  =                 61.5 / Value at ref. pixel on axis 1
CRVAL2  =                 63.5 / Value at ref. pixel on axis 2
CTYPE1  = 'CCD_REG1'           / Quantity represented by axis 1
CTYPE2  = 'CCD_REG2'           / Quantity represented by axis 2
CD1_1   =                  1.0 / Transformation matrix element
CD2_2   =                  1.0 / Transformation matrix element
EXTEND  =                    T / FITS dataset may contain extensions
COMMENT   FITS (Flexible Image Transport System) format is defined in 'Astronomy
COMMENT   and Astrophysics', volume 376, page 359; bibcode: 2001A&A...376..359H
LBOUND1 =                    6 / Pixel origin along axis 1
LBOUND2 =                    1 / Pixel origin along axis 2
OBJECT  = 'Output from TRANNDF'/ Title of the dataset
DATE    = '2004-02-22T22:02:27'/ file creation date (YYYY-MM-DDThh:mm:ss UT)
ORIGIN  = 'Starlink Project, U.K.'/ Origin of this FITS file
BSCALE  =                  1.0 / True_value = BSCALE * FITS_value + BZERO
BZERO   =                  0.0 / True_value = BSCALE * FITS_value + BZERO
HDUCLAS1= 'NDF     '           / Starlink NDF (hierarchical n-dim format)
HDUCLAS2= 'DATA    '           / Array component subclass
CCDXIMSI=                  114
CCDXIMST=                    6
CCDXSIZE=                  128
CCDYIMSI=                  128
CCDYIMST=                    1
CCDYSIZE=                  128
GAIN    =                    1
READNOIS=                 10.0
PFMFNAME= 'B                 '
OBSTYPE = 'TARGET            '
TELESCOP= 'CCDPACK SPECIAL   '
ISEQ    =                    1
END
