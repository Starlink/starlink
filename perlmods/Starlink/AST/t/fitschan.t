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
my $wcsinfo = $fchan->Read();

$wcsinfo->Show();
print "WCSINFO is $wcsinfo\n";

Starlink::AST::End();

print "Shutting down\n";

__DATA__
SIMPLE  =                    T / file does conform to FITS standard
BITPIX  =                  -32 / number of bits per data pixel
NAXIS   =                    3 / number of data axes
NAXIS1  =                   25 / length of data axis 1
NAXIS2  =                   36 / length of data axis 2
NAXIS3  =                  252 / length of data axis 3
EXTEND  =                    T / FITS dataset may contain extensions
COMMENT   FITS (Flexible Image Transport System) format defined in Astronomy and
COMMENT   Astrophysics Supplement Series v44/p363, v44/p371, v73/p359, v73/p365.
COMMENT   Contact the NASA Science Office of Standards and Technology for the
COMMENT   FITS Definition document #100 and other FITS information.
CRVAL1  = -0.07249999791383749 / Axis 1 reference value
CRPIX1  =                 12.5 / Axis 1 pixel value
CTYPE1  = 'a1      '           / LINEAR
CRVAL2  = -0.07249999791383743 / Axis 2 reference value
CRPIX2  =                 18.0 / Axis 2 pixel value
CTYPE2  = 'a2      '           / LINEAR
CRVAL3  =  1.27557086671004E-6 / Axis 3 reference value
CRPIX3  =                126.0 / Axis 3 pixel value
CTYPE3  = 'a3      '           / LAMBDA
OBJECT  = 'galaxy  '           / Title of the dataset
DATE    = '2000-12-13T22:44:53' / file creation date (YYYY-MM-DDThh:mm:ss UTC)
ORIGIN  = 'NOAO-IRAF FITS Image Kernel July 1999' / FITS file originator
BSCALE  =                  1.0 / True_value = BSCALE * FITS_value + BZERO
BZERO   =                  0.0 / True_value = BSCALE * FITS_value + BZERO
HDUCLAS1= 'NDF     '           / Starlink NDF (hierarchical n-dim format)
HDUCLAS2= 'DATA    '           / Array component subclass
IRAF-TLM= '23:07:26 (27/02/2000)' / Time of last modification
TELESCOP= 'UKIRT, Mauna Kea, HI' / Telescope name
INSTRUME= 'CGS4    '           / Instrument
OBSERVER= 'SMIRF   '           / Observer name(s)
OBSREF  = '?       '           / Observer reference
DETECTOR= 'fpa046  '           / Detector array used
OBSTYPE = 'OBJECT  '           / Type of observation
INTTYPE = 'STARE+NDR'          / Type of integration
