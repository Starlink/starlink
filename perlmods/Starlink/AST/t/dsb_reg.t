#!perl

# Tests based on David Berry's Fortran regression code
# Doesn't really test anything other than whether it runs at the moment.
# Problem is that Dave's Fortran code uses default fortran formatting
# which is difficult to replicate in perl with sprintf.

use strict;
use Test::More tests => 3;

$| = 1;

require_ok("Starlink::AST");

# Read the FITS headers from the data handle.
# and put them in an array of arrays.
my @headers;
my $index = 0;
while (<DATA>) {
  $headers[$index] = [] if not defined $headers[$index];
  if ($_ =~ /^---/) {
    $index++;
    next;
  }
  push(@{$headers[$index]}, $_);
}

# make sure we read the right number
is( @headers, 3, "Number of headers read");

# These are the parameters from the top of the fortran code
my @cat = ( 'Colour(axis1)', 'Font(Stri)', 'Nout', 'Class' );
my @rat = ( 'Tol', 'Gap(1)' );
my @lat = ( 'Border', 'Invert' );
my @dat = ( 'TextLabGap' );
my @iat = ( 'Nin', 'Current', 'Base', 'Nobject', 'RefCOUNT' );
my @gbox = ( -100.0, -200.0, 150.0, 300.0 );
my @bbox = ( [ 10.0, -10.0, 290.0, 300.0 ],
	     [ -300.0, -300.0, 500.0, 500.0 ],
	     [ 1.0, 1.0, 1787.0, 447.0]
	   );
my @attrs = ( 'Grid=1,tickall=0',
	      'Grid=1,labelling=interior',
	      'Grid=0' );

Starlink::AST::Begin();

# loop over all tests
for my $i ( 0 .. $#headers ) {

  print "# \n";
  print "# FITS test number ". ($i+1). "\n";
  print "# ====================\n";
  print "# \n";

  # Create a fitschan, read an object from it and dump the object
  # to standard output. The object should be a frameset if all is OK
  my $fc = new Starlink::AST::FitsChan(
		           source => sub { my $line = shift(@{$headers[$i]});
					   chomp($line) if $line;
					   $line;
					 },
			   sink => \&reg_sink,
				      );
  my $fs = $fc->Read();
  print "# \n";
  print "# AST_SHOW:\n";
  $fs->Show();

  # Annul the FitsChan. This will cause the unused contents (if any) to
  # be written out using the sink callback
  print "# \n";
  print "# SINK:\n";
  $fc->Annul();

  # Create another FrameSet with Native encoding. Write the FrameSet to
  # it, and then annul the FitsChan (this will cause the FITS cards to be
  # written to stdout).
  $fc = new Starlink::AST::FitsChan( sink => \&reg_sink,
				     Encoding => 'native' );
  print "# \n";
  print "# Objects written: ", $fc->Write( $fs ) ."\n";
  print "# \n";
  print "# Native Encoding:\n";
  $fc->Annul();

  # Create a Plot which maps the area specified by BBOX the Base Frame
  # of the FrameSet onto the GBOX area in graphics coords.
  my $plot = new Starlink::AST::Plot( $fs, \@gbox, $bbox[$i],
				      "title= A FITS test, tol = 0.01"
				    );

  # Annul the FraemSet
  $fs->Annul();

  # Register internal plot functions
  $plot->GAttr( \&reg_attr );
  $plot->GFlush( \&reg_flush );
  $plot->GLine( \&reg_line );
  $plot->GMark( \&reg_mark );
  $plot->GText( \&reg_text );
  $plot->GTxExt( \&reg_txext );
  $plot->GScales( \&reg_scales );
  $plot->GCap( \&reg_cap );
  $plot->GQch( \&reg_qch );

  # Set some attributes
  $plot->Set( $attrs[$i] );

  # Get some attributes
  for my $attr ( @cat ) {
    print "$attr: ". $plot->Get( $attr ) . "\n";
  }
  for my $attr ( @rat ) {
    print "$attr: ". $plot->Get( $attr ) . "\n";
  }
  for my $attr ( @lat ) {
    print "$attr: ". $plot->Get( $attr ) . "\n";
  }
  for my $attr ( @dat ) {
    print "$attr: ". $plot->Get( $attr ) . "\n";
  }
  for my $attr ( @iat ) {
    print "$attr: ". $plot->Get( $attr ) . "\n";
  }

  # Draw a grid
  print "# \n";
  print "# AST_GRID:\n";
  $plot->Grid();

}
Starlink::AST::End();
ok(1, "Made it to the end");

exit;

# Functions

# Dump FITS header cards to STDOUT
sub reg_sink {
  my $line = shift;
  print "$line\n";
}

# Graphics callbacks
sub reg_flush {
  print "# REG_FLUSH\n";
  return 1;
}

my @gattrs;
sub reg_attr {
  my ($att, $val, $prim ) = @_;
  print "# REG_GATTR: $att, $val, $prim\n";

  my $MAX_ATTR = 5;
  my $i;
  if ($att == &Starlink::AST::Grf::GRF__STYLE() ) {
    $i = 1;
  } elsif ( $att = &Starlink::AST::Grf::GRF__WIDTH() ) {
    $i = 2;
  } elsif ( $att = &Starlink::AST::Grf::GRF__SIZE() ) {
    $i = 3;
  } elsif ( $att = &Starlink::AST::Grf::GRF__FONT() ) {
    $i = 4;
  } elsif ( $att = &Starlink::AST::Grf::GRF__COLOUR() ) {
    $i = 5;
  } else {
    print "Bad ATT value: ", $att ."\n";
  }

  my $j;
  if ($prim == &Starlink::AST::Grf::GRF__LINE() ) {
    $j = 1;
  } elsif ($prim == &Starlink::AST::Grf::GRF__MARK() ) {
    $j = 2;
  } elsif ($prim == &Starlink::AST::Grf::GRF__TEXT() ) {
    $j = 3;
  } else {
    print "# Bad PRIM value: $prim\n";
  }

  # Store the new value if required
  # Convert prim and att to index of 2d array
  my $index = ( $MAX_ATTR * ($att - 1) + $prim );
  my $old = $gattrs[$index];
  $old = &Starlink::AST::AST__BAD() if !defined $old;
  $gattrs[$index] = $val if $val != &Starlink::AST::AST__BAD();

  return (1, $old);

}

sub reg_line {
  my ($x, $y) = @_;
  my $n = scalar(@$x);
  print "# REG_LINE: $n\n";
  for my $i ( 1 .. $n ) {
    print "    ". $x->[$i-1]. " ", $y->[$i-1] ."\n";
  }
  return 1;
}

sub reg_mark {
  my ($x, $y, $type) = @_;
  my $n = scalar(@$x);
  print "# REG_MARK: $n  $type\n";
  for my $i ( 1 .. $n ) {
    print "    ". $x->[$i-1]. " ", $y->[$i-1] ."\n";
  }
  return 1;
}

sub reg_text {
  my ($text, $x, $y, $just, $upx, $upy) = @_;

  print "# REG_TEXT: '$text'\n";
  printf "#    %f %f, '%s', %.f %.f\n", $x, $y, $just,$upx,$upy;
  return 1;
}

sub reg_txext {
  my ($text, $x, $y, $just, $upx, $upy) = @_;

  my (@xb, @yb);
  $xb[0] = $x - length($text) * 0.5;
  $xb[1] = $x + length($text) * 0.5;
  $xb[2] = $xb[1];
  $xb[3] = $xb[0];

  $yb[0] = $y - 0.5;
  $yb[1] = $yb[0];
  $yb[2] = $y + 0.5;
  $yb[3] = $yb[2];

  return (1, \@xb, \@yb);
}

sub reg_cap {
  my ($cap, $value) = @_;
  printf "# REG_CAP: %2d\n", $cap;
  return ($cap == &Starlink::AST::Grf::GRF__SCALES() ? 1 : 0 );
}

sub reg_scales {
  print "# REG_SCALES: \n";
  return (1,1,1);
}

sub reg_qch {
  print "# REG_QCH: \n";
  return (1, 0.01, 0.01);
}


__DATA__
SIMPLE  =                    T / Written by IDL:  30-Jul-1997 05:35:42.00
BITPIX  =                  -32 / Bits per pixel.
NAXIS   =                    2 / Number of dimensions
NAXIS1  =                  300 / Length of x axis.
NAXIS2  =                  300 / Length of y axis.
CTYPE1  = 'GLON-ZEA'           / X-axis type
CTYPE2  = 'GLAT-ZEA'           / Y-axis type
CRVAL1  =           -149.56866 / Reference pixel value
CRVAL2  =           -19.758201 / Reference pixel value
CRPIX1  =              150.500 / Reference pixel
CRPIX2  =              150.500 / Reference pixel
CDELT1  =             -1.20000 / Degrees/pixel
CDELT2  =              1.20000 / Degrees/pixel
CROTA1  =              0.00000 / Rotation in degrees.
COMMENT
COMMENT This file was produced by the SkyView survey analysis system from
COMMENT available astronomical surveys.  The data are formatted
COMMENT as a simple two-dimensional FITS image with the same units as
COMMENT the orginal survey.  A single ASCII table extension may be present
COMMENT which describes catalog objects found within the field of view.
COMMENT Copies of relevant copyright notices are included in this file.
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
SURVEY  = 'COBE DIRBE'
BUNITS  = 'MJy/sr  '           /
ORIGIN  = 'CDAC    '           / Cosmology Data Analysis Center
TELESCOP= 'COBE    '           / COsmic Background Explorer satellite
INSTRUME= 'DIRBE   '           / COBE instrument [DIRBE, DMR, FIRAS]
PIXRESOL=                    9 / Quad tree pixel resolution [6, 9]
DATE    = '27/09/94'           / FITS file creation date (dd/mm/yy)
DATE-MAP= '16/09/94'           / Date of original file creation (dd/mm/yy)
COMMENT     COBE specific keywords
DATE-BEG= '08/12/89'           / date of initial data represented (dd/mm/yy)
DATE-END= '25/09/90'           / date of final data represented   (dd/mm/yy)
COMMENT
COMMENT THE COBE DIRBE map is a combination of the original ten
COMMENT band passes with the following wavelengths:
COMMENT     Band 1  - 1.25 microns
COMMENT     Band 2  - 2.2  microns
COMMENT     Band 3  - 3.5  microns
COMMENT     Band 4  - 4.9  microns
COMMENT     Band 5  -  12  microns
COMMENT     Band 6  -  25  microns
COMMENT     Band 7  -  60  microns
COMMENT     Band 8  - 100  microns
COMMENT     Band 9  - 140  microns
COMMENT     Band 10 - 240  microns
COMMENT
END
---
COMMENT AST ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
AST
COMMENT AST            Beginning of AST data for FrameSet object
AST
COMMENT AST ................................................................
AST
BEGAST_A= 'FrameSet'           / Set of inter-related coordinate systems
NFRAME_A=                    2 / Number of Frames in FrameSet
CURRNT_A=                    2 / Index of current Frame
NOD1_A  =                    2 / Frame 1 is associated with node 2
NOD2_A  =                    1 / Frame 2 is associated with node 1
LNK2_A  =                    1 / Node 2 is derived from node 1
FRM1_A  = '        '           / Frame number 1
BEGAST_B= 'Frame   '           / Coordinate system description
TITLE_A = 'Data grid indices; first pixel at (1&'/ Title of coordinate system
CONTINUE  ',1)     '
NAXES_A =                    2 / Number of coordinate axes
DOMAIN_A= 'GRID    '           / Coordinate system domain
AX1_A   = '        '           / Axis number 1
BEGAST_C= 'Axis    '           / Coordinate axis
LABEL_A = 'Data grid index 1'  / Axis Label
SYMBOL_A= 'g1      '           / Axis symbol
UNIT_A  = 'pixel   '           / Axis units
FORMAT_A= '%3.1f   '           / Format specifier
ENDAST_A= 'Axis    '           / End of object definition
AX2_A   = '        '           / Axis number 2
BEGAST_D= 'Axis    '           / Coordinate axis
LABEL_B = 'Data grid index 2'  / Axis Label
SYMBOL_B= 'g2      '           / Axis symbol
UNIT_B  = 'pixel   '           / Axis units
FORMAT_B= '%3.1f   '           / Format specifier
ENDAST_B= 'Axis    '           / End of object definition
ENDAST_C= 'Frame   '           / End of object definition
FRM2_A  = '        '           / Frame number 2
BEGAST_E= 'Frame   '           / Coordinate system description
TITLE_B = 'Pixel coordinates; first pixel at (-&'/ Title of coordinate system
CONTINUE  '100.5,-200.5)'
NAXES_B =                    2 / Number of coordinate axes
DOMAIN_B= 'POLAR   '           / Coordinate system domain
AX1_B   = '        '           / Axis number 1
BEGAST_F= 'Axis    '           / Coordinate axis
LABEL_C = 'Pixel coordinate 1' / Axis Label
SYMBOL_C= 'p1      '           / Axis symbol
UNIT_C  = 'pixel   '           / Axis units
FORMAT_C= '%3.1f   '           / Format specifier
ENDAST_D= 'Axis    '           / End of object definition
AX2_B   = '        '           / Axis number 2
BEGAST_G= 'Axis    '           / Coordinate axis
LABEL_D = 'Pixel coordinate 2' / Axis Label
SYMBOL_D= 'p2      '           / Axis symbol
UNIT_D  = 'pixel   '           / Axis units
FORMAT_D= '%3.1f   '           / Format specifier
ENDAST_E= 'Axis    '           / End of object definition
ENDAST_F= 'Frame   '           / End of object definition
MAP2_A  = '        '           / Mapping between nodes 1 and 2
BEGAST_H= 'CmpMap  '           / Compound Mapping
NIN_A   =                    2 / Number of input coordinates
ISA_A   = 'Mapping '           / Mapping between coordinate systems
INVA_A  =                    1 / First Mapping used in inverse direction
INVB_A  =                    1 / Second Mapping used in inverse direction
MAPA_A  = '        '           / First component Mapping
BEGAST_I= 'MathMap '           / Transformation using mathematical functions
NIN_B   =                    2 / Number of input coordinates
INVERT_A=                    0 / Mapping not inverted
ISA_B   = 'Mapping '           / Mapping between coordinate systems
FWD1_A  = 'r=sqrt(x*x+y*y)'    / Forward function 1
FWD2_A  = 'theta=atan2(y,x)'   / Forward function 2
INV1_A  = 'x=r*cos(theta)'     / Inverse function 1
INV2_A  = 'y=r*sin(theta)'     / Inverse function 2
SIMPFI_A=                    1 / Forward-inverse pairs may simplify
SIMPIF_A=                    1 / Inverse-forward pairs may simplify
ENDAST_G= 'MathMap '           / End of object definition
MAPB_A  = '        '           / Second component Mapping
BEGAST_J= 'WinMap  '           / Map one window on to another
NIN_C   =                    2 / Number of input coordinates
INVERT_B=                    0 / Mapping not inverted
ISA_C   = 'Mapping '           / Mapping between coordinate systems
SFT1_A  =               -101.5 / Shift for axis 1
SFT2_A  =               -201.5 / Shift for axis 2
ENDAST_H= 'WinMap  '           / End of object definition
ENDAST_I= 'CmpMap  '           / End of object definition
ENDAST_J= 'FrameSet'           / End of object definition
COMMENT AST ................................................................
AST
COMMENT AST               End of AST data for FrameSet object
AST
COMMENT AST ----------------------------------------------------------------
AST
---
SIMPLE  =                    T / file does conform to FITS standard
BITPIX  =                   16 / number of bits per data pixel
NAXIS   =                    2 / number of data axes
NAXIS1  =                 1787 / length of data axis 1
NAXIS2  =                  447 / length of data axis 2
EXTEND  =                    T / FITS dataset may contain extensions
COMMENT   FITS (Flexible Image Transport System) format defined in Astronomy and
COMMENT   Astrophysics Supplement Series v44/p363, v44/p371, v73/p359, v73/p365.
COMMENT   Contact the NASA Science Office of Standards and Technology for the
COMMENT   FITS Definition document #100 and other FITS information.
PLATENUM= '3665    '           / Plate number
EMULSION= 'IIIaJ   '           / Kodak emulsion type
FILTER  = 'GG395   '           / Schott glass filter type
PLTSCALE= '67.14   '           / [arcsec/mm] plate scale
FIELDNUM= '1       '           / Sky survey field number
EPOCH   =         1.977780E+03 / Epoch of observation
DATE-OBS= '1977-10-11'         / [yyyy-mm-dd] UT date of observation
TELESCOP= 'UKST    '           / Telescope on which the plate was taken
TELETYPE= 'SCHM    '           / Type of telescope
SITELAT =  -5.458410576565E-01 / [radians] latitude of telescope
SITELONG=   2.601766194458E+00 / [radians] longitude of telescope
LST     = '00:20   '           / [hh:mm] local sidereal time at start of obs
MJD-OBS =   4.342657300880E+04 / Modified Julian Date of observation
INSTRUME= 'SuperCOSMOS I'      / Measuring machine
DATE-MES= '2000-11-04'         / [yyyy-mm-dd] Date of this plate measurement
RADECSYS= 'FK5     '           / Reference frame for RA/DEC in original file
NHKLINES=                  146 / Number of lines from house-keeping file
HKLIN001= 'JOB.JOBNO                UKJ001' /
HKLIN002= 'JOB.DATE-MES             2000:11:04' /
HKLIN003= 'JOB.TIME                 12:51:09' /
HKLIN004= 'JOB.INSTRUME             SuperCOSMOS I' /
HKLIN005= 'JOB.ORIGIN               Royal Observatory Edinburgh' /
HKLIN006= 'JOB.SOFTWARE             /home/scosdev/v033' /
HKLIN007= 'JOB.OPERATOR             ebt' /
HKLIN008= 'JOB.USER                 htm' /
HKLIN009= 'JOB.USERREF              NONE' /
HKLIN010= 'JOB.UORIGIN              ROE' /
HKLIN011= 'JOB.UCOUNTRY             uk' /
HKLIN012= 'JOB.COMMENT              Digital catalogue of the Sky' /
HKLIN013= 'JOB.IAM_FILE             iam.srt'//            ' /
HKLIN014= 'PLATE.TELESCOP           UKST' /
HKLIN015= 'PLATE.TELTYPE            SCHM' /
HKLIN016= 'PLATE.PLATE              3665' /
HKLIN017= 'PLATE.MATERIAL           3mm glass' /
HKLIN018= 'PLATE.EMULSION           IIIaJ' /
HKLIN019= 'PLATE.FILTER             GG395' /
HKLIN020= 'PLATE.PSCALE             67.14' /
HKLIN021= 'PLATE.FIELD              1' /
HKLIN022= 'PLATE.RA_PNT             0' /
HKLIN023= 'PLATE.DEC_PNT            -90' /
HKLIN024= 'PLATE.RADECSYS           FK4' /
HKLIN025= 'PLATE.EQUINOX            1950' /
HKLIN026= 'PLATE.TIMESYS            BESSELIAN' /
HKLIN027= 'PLATE.EPOCH              1977.78'//            ' /
HKLIN028= 'PLATE.EXPOSURE           75' /
HKLIN029= 'PLATE.UTDATE             771011' /
HKLIN030= 'PLATE.LST                0020' /
HKLIN031= 'PLATE.MJD                43426.573008796' /
HKLIN032= 'PLATE.TELLAT             -0.54584105765654' /
HKLIN033= 'PLATE.TELLONG            2.6017661944583' /
HKLIN034= 'PLATE.TELHT              1145' /
HKLIN035= 'PLATE.TEMP               273.155'//            ' /
HKLIN036= 'PLATE.ATMOSP             1013.25'//            ' /
HKLIN037= 'PLATE.HUMID              0.5' /
HKLIN038= 'PLATE.WAVE               4500' /
HKLIN039= 'PLATE.TROPL              0.0065' /
HKLIN040= 'CALIBRATION.CALTYPE      SPLINE' /
HKLIN041= 'CALIBRATION.STEPWEDG     KPNO' /
HKLIN042= 'CALIBRATION.NSTEPS       8' /
HKLIN043= 'MEASUREMENT.ORIENTAT     news' /
HKLIN044= 'MEASUREMENT.EMULPOS      UP' /
HKLIN045= 'MEASUREMENT.SCANFILT     14' /
HKLIN046= 'MEASUREMENT.SOSP         552' /
HKLIN047= 'MEASUREMENT.STEPSIZE     10' /
HKLIN048= 'MEASUREMENT.SCANLEN      1152' /
HKLIN049= 'MEASUREMENT.A-XMIN       1622000'//            ' /
HKLIN050= 'MEASUREMENT.A-YMIN       1622000'//            ' /
HKLIN051= 'MEASUREMENT.A-XMAX       33878000' /
HKLIN052= 'MEASUREMENT.A-YMAX       33878000' /
HKLIN053= 'MEASUREMENT.X_PNT        17500000' /
HKLIN054= 'MEASUREMENT.Y_PNT        18000000' /
HKLIN055= 'ANALYSIS.NPARAMS         32' /
HKLIN056= 'ANALYSIS.AREACUT         8' /
HKLIN057= 'ANALYSIS.AP-PARAM        1.07' /
HKLIN058= 'DEBLEND.DB-PARAM         1.05' /
HKLIN059= 'DEBLEND.DB-AMIN          16' /
HKLIN060= 'DEBLEND.DB-AMAX          100000' /
HKLIN061= 'DEBLEND.DB-ACUT          8' /
HKLIN062= 'DEBLEND.DB-LEVEL         16' /
HKLIN063= 'DEBLEND.SELECT           PARENT+CHILD' /
HKLIN064= 'SKY.SKYSQUAR             64' /
HKLIN065= 'SKY.SKYDEFN              MEDIAN' /
HKLIN066= 'SKY.SKYFILTR             bdkjunk'//            ' /
HKLIN067= 'SKY.F-THRESH             8' /
HKLIN068= 'SKY.F-SCLEN              4' /
HKLIN069= 'THRESHOLDING.PCUT        10' /
HKLIN070= 'IAMQC.AREAMIN            8' /
HKLIN071= 'IAMQC.AREAMAX            77346' /
HKLIN072= 'IAMQC.MINMAG             -30515' /
HKLIN073= 'IAMQC.MAXMAG             -17954' /
HKLIN074= 'IAMQC.MINELL             0.0004156232' /
HKLIN075= 'IAMQC.MAXELL             1' /
HKLIN076= 'IAMQC.MODELL             0.14' /
HKLIN077= 'IAMQC.MODOR              91' /
HKLIN078= 'IAMQC.MIDELL             0.21' /
HKLIN079= 'IAMQC.MIDOR              93' /
HKLIN080= 'IAMQC.MEANELL            0.2467037' /
HKLIN081= 'IAMQC.MEANOR             91.63474' /
HKLIN082= 'IAMQC.NUMOBJ             556985' /
HKLIN083= 'IAMQC.PARENTS            486656' /
HKLIN084= 'IAMQC.RANGING            TRUE' /
HKLIN085= 'IAMQC.LANE_1             15571' /
HKLIN086= 'IAMQC.LANE_2             33207' /
HKLIN087= 'IAMQC.LANE_3             51478' /
HKLIN088= 'IAMQC.LANE_4             69944' /
HKLIN089= 'IAMQC.LANE_5             89236' /
HKLIN090= 'IAMQC.LANE_6             108416' /
HKLIN091= 'IAMQC.LANE_7             127481' /
HKLIN092= 'IAMQC.LANE_8             146699' /
HKLIN093= 'IAMQC.LANE_9             166380' /
HKLIN094= 'IAMQC.LANE_10            186126' /
HKLIN095= 'IAMQC.LANE_11            205946' /
HKLIN096= 'IAMQC.LANE_12            225915' /
HKLIN097= 'IAMQC.LANE_13            245926' /
HKLIN098= 'IAMQC.LANE_14            266574' /
HKLIN099= 'IAMQC.LANE_15            287150' /
HKLIN100= 'IAMQC.LANE_16            308087' /
HKLIN101= 'IAMQC.LANE_17            328830' /
HKLIN102= 'IAMQC.LANE_18            350253' /
HKLIN103= 'IAMQC.LANE_19            370738' /
HKLIN104= 'IAMQC.LANE_20            391722' /
HKLIN105= 'IAMQC.LANE_21            412801' /
HKLIN106= 'IAMQC.LANE_22            433795' /
HKLIN107= 'IAMQC.LANE_23            454383' /
HKLIN108= 'IAMQC.LANE_24            474711' /
HKLIN109= 'IAMQC.LANE_25            495108' /
HKLIN110= 'IAMQC.LANE_26            515755' /
HKLIN111= 'IAMQC.LANE_27            536499' /
HKLIN112= 'IAMQC.LANE_28            556985' /
HKLIN113= 'XYTORADEC.STARCAT        /sdata/scos/refcats/tycho2.FIT' /
HKLIN114= 'XYTORADEC.BRIGHTLIM      9' /
HKLIN115= 'XYTORADEC.C-EQUIN        2000' /
HKLIN116= 'XYTORADEC.C-EQTSYS       JULIAN' /
HKLIN117= 'XYTORADEC.C-EPOCH        2000' /
HKLIN118= 'XYTORADEC.C-EPTSYS       JULIAN' /
HKLIN119= 'XYTORADEC.R-EQUIN        2000' /
HKLIN120= 'XYTORADEC.R-TSYS         JULIAN' /
HKLIN121= 'XYTORADEC.MAXITER        5000' /
HKLIN122= 'XYTORADEC.RCRITINI       500000' /
HKLIN123= 'XYTORADEC.RCRITABS       50000' /
HKLIN124= 'XYTORADEC.RCRITREL       1' /
HKLIN125= 'XYTORADEC.RCRITFIN       3' /
HKLIN126= 'XYTORADEC.HARDCOPY       /scos1/scos/UKJ001/UKJ001.ps' /
HKLIN127= 'XYTORADEC.REFSMULT       5' /
HKLIN128= 'XYTORADEC.RESDMULT       1000' /
HKLIN129= 'XYTORADEC.RACOL          RA' /
HKLIN130= 'XYTORADEC.DECOL          DEC' /
HKLIN131= 'XYTORADEC.RAPMCOL        PMRA' /
HKLIN132= 'XYTORADEC.DECPMCOL       PMDE' /
HKLIN133= 'XYTORADEC.PLXCOL         NONE' /
HKLIN134= 'XYTORADEC.RVCOL          NONE' /
HKLIN135= 'XYTORADEC.MAGCOL         VT' /
HKLIN136= 'XYTORADEC.STARSC         2374' /
HKLIN137= 'XYTORADEC.STARSU         1727' /
HKLIN138= 'XYTORADEC.COEFFS_1       17.640343856524' /
HKLIN139= 'XYTORADEC.COEFFS_2       -260.44151995641' /
HKLIN140= 'XYTORADEC.COEFFS_3       -163.09155572601' /
HKLIN141= 'XYTORADEC.COEFFS_4       17.504230442205' /
HKLIN142= 'XYTORADEC.COEFFS_5       -163.08676953832' /
HKLIN143= 'XYTORADEC.COEFFS_6       260.48817907668' /
HKLIN144= 'XYTORADEC.DISTR          -0.33333333333333' /
HKLIN145= 'XYTORADEC.RA_PNT         0.54924996662137' /
HKLIN146= 'XYTORADEC.DEC_PNT        -1.5684931501781' /
HISTORY = 'SuperCOSMOS image analysis and mapping mode (IAM and MM)' /
HISTORY = 'data written by xydcomp_ss.' /
HISTORY = 'Any questions/comments/suggestions/bug reports should be sent' /
HISTORY = 'to N.Hambly@roe.ac.uk' /
ASTSIGX =         3.700000E-01 / [arcsec] std. dev. of astrometric fit in X
ASTSIGY =         3.800000E-01 / [arcsec] std. dev. of astrometric fit in Y
CRVAL1  =   0.000000000000E+00 / Axis 1 reference value
CRPIX1  =   8.936318379289E+02 / Axis 1 pixel value
CTYPE1  = 'RA---TAN'           / Quantity represented by axis 1
CRVAL2  =  -9.000000018364E+01 / Axis 2 reference value
CRPIX2  =   2.238380193875E+02 / Axis 2 pixel value
CTYPE2  = 'DEC--TAN'           / Quantity represented by axis 2
CD1_1   =  -1.864642639667E-04 / Co-ordinate transformation matrix
CD1_2   =  -9.188369023766E-07 / Co-ordinate transformation matrix
CD2_1   =  -1.038232462415E-06 / Co-ordinate transformation matrix
CD2_2   =   1.866269837741E-04 / Co-ordinate transformation matrix
CDELT1  =  -1.864665278217E-04 / DEPRECATED - Increment per pixel on axis 1
CDELT2  =   1.866298716692E-04 / DEPRECATED - Increment per pixel on axis 2
PC001001=   9.999878591881E-01 / DEPRECATED - Axis rotation matrix
PC001002=   4.927623810613E-03 / DEPRECATED - Axis rotation matrix
PC002001=  -5.563056187788E-03 / DEPRECATED - Axis rotation matrix
PC002002=   9.999845260832E-01 / DEPRECATED - Axis rotation matrix
CROTA2  =   3.005532298491E-01 / DEPRECATED - rotation of axis 2
EQUINOX =         2.000000E+03 / Julian reference frame equinox
DATATYPE= 'INTEGER*2'          / Type of data
DATUNITS= 'DENSITY '           / Units: transmission, density or intensity
XPIXELSZ=   9.997114974000E+00 / [microns] X pixel size
YPIXELSZ=   1.000000000000E+01 / [microns] Y pixel size
OBJCTRA = '  0  0  0.000'      / Centre Right Ascension (J2000)
OBJCTDEC= '-90  0  0.00'       / Centre Declination (J2000)
OBJCTX  =   1.636863183793E+04 / [pixels] Centre X on plate
OBJCTY  =   1.474083801939E+04 / [pixels] Centre Y on plate
END
