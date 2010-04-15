#!perl

use strict;
use Test::More tests => 10;

require_ok("Starlink::AST");

Starlink::AST::Begin();

my $fchan = new Starlink::AST::FitsChan();
while (<DATA>) {
  $fchan->PutFits($_ ,0);
}
$fchan->Clear( "Card" );

# Get FrameSet
my $wcsinfo = $fchan->Read();
isa_ok( $wcsinfo, "Starlink::AST::FrameSet" );

my ( $offset, @p1, @p2 );
my $point1 = \@p1;
my $point2 = \@p2;

$p1[0] = 0;
$p1[1] = 0;
$p2[0] = 114;
$p2[1] = 128;

# Distance
# --------
my $point3 = $wcsinfo->Distance( $point1, $point2 );
is( sprintf( "%.1f", $point3), 171.4,
    "Distance between points using Distance()" );

$offset = 5;
$p1[0] = 0;
$p1[1] = 0;
$p2[0] = 8;
$p2[1] = 6;

# Offset
# ------
$point3 = $wcsinfo->Offset( $point1, $point2, $offset );
is( $$point3[0], 4, "X distance using Offset()" );
is( $$point3[1], 3, "Y distance using Offset()" );

# Offset2
# -------
my $angle = atan2(4, 3);
my $actual;
($actual, $point3) = $wcsinfo->Offset2( $point1, $angle, $offset );
is( $actual, $angle, "Angle using Offset2()" );
is( $$point3[0], 4, "X distance using Offset2()" );
is( $$point3[1], 3, "Y distance using Offset2()" );

# Resolve
# -------
my ($p4, $d1, $d2) = $wcsinfo->Resolve( [3,3],[6,7],[8,3] );
is($d1, 3, "Distance from basis vector");
is($d2, 4, "Distance from basis vector");


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
