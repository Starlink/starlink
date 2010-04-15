#!/usr/bin/perl
#
# S C U P O L
#
# Calculate imaging polarimetry from SCUBA data.
# Each image has a subset of 16 embedded images. These 16 images are split
# into the following (effective) sequence : 0, 22.5, 45, 67.5 degrees.
# In fact the w/plate makes a complete rotation, sampling at every 22.5 deg
# of the waveplate.
#

#  Authors:
#    Antonio Chrysostomou (JAC/U.Herts)
#    Tim Jenness (JAC)


#  Copyright:
#     Copyright (C) 1998,1999 Particle Physics and Astronomy
#     Research Council. All Rights Reserved.


#
# 5-Aug-1998 : ACC (modified to Perl from original C-Shell script)
#
# 20-Nov-1998: ACC (modified for smoothing data by setting the
#                     REBIN_METHOD=gaussian parameter of the
#                           intrebin calculations)
# 11-FEB-1999: TJ (use new SURF REMIP script)
#                 (Run GLITCH on images before calculating q,u,i)
#

# Welcome
print " \n";
print "                   S C U P O L \n";
print "                   ^^^^^^^^^^^ \n";
print " \n";
print "    A Data Reduction Recipe for SCUBA Polarimetry\n";
print " \n";
print "               Antonio Chrysostomou \n";
print "  Joint Astronomy Centre & University of Hertfordshire\n";
print " \n";

use NDF;
use Math::Trig;
use constant PI => (4*atan2(1,1));
use constant D2R => (PI/180.0);
$| = 1; # unbuffer output

# Check that DATADIR and SCUBA_PREFIX environment variables are set
$check = 0;
if ( ! $ENV{DATADIR} ) {
  $check = 1;
}
if (! $ENV{SCUBA_PREFIX} ) {
  $check += 2;
}
if ( $check == 1 ) {
  print "\nPlease set the DATADIR environment variable.\n";
  print "  DATADIR should point to the directory where the demodulated data are kept.\n";
  exit;
}
if ( $check == 2 ) {
  print "\nPlease set the SCUBA_PREFIX environment variable.\n";
  print "  SCUBA_PREFIX should be the UT date that the observations were made (yyyymmdd).\n";
  exit;
}
if ( $check == 3 ) {
  print "\nPlease set the DATADIR and SCUBA_PREFIX environment variables.\n";
  print "  DATADIR should point to the directory where the demodulated data are kept.\n";
  print "  SCUBA_PREFIX should be the UT date that the observations were made (yyyymmdd).\n";
  exit;
}
print "\nDATADIR : $ENV{DATADIR}";
print "\nSCUBA_PREFIX : $ENV{SCUBA_PREFIX}\n";

# Get the SCUBA filenumber
if ( ! defined ($ARGV[0]) ) {
  print " File number : ";
  chomp ($fnum = <STDIN>);
} else {
  chomp ($fnum = $ARGV[0]) ;
}

$padfnum = '0'x(4-length($fnum)).$fnum;

# Get the CSO tau number from the fits header
($hashref, $status) = fits_read_header("$ENV{DATADIR}/$ENV{SCUBA_PREFIX}_dem_$padfnum");
die "Nope....couldn't find it!\n" unless $status == &NDF::SAI__OK;
$csotau = $$hashref{TAU_225};
$tau = 4.3 * ($csotau - 0.007);

# Get the smoothing scale
print "\n\n Give the smoothing scale (FWHM, arcsec) [10] : ";
chomp ($ans = <STDIN> );
if ( $ans ne "" ) {
  $smooth = $ans / 2.0 ;
} else {
  $smooth = 5;
}

# Quick SCUBA reduction
system ("$ENV{SURF_DIR}/scuquick -quick -rebin -sub long -tau $tau $fnum out=o${fnum} PIXSIZE_OUT=3 ");
#system ("$ENV{KAPPA_DIR}/display axes clear o${fnum}_lon_reb mode=per accept");
#system ("$ENV{KAPPA_DIR}/lutgrey device=xwindows");
system ("$ENV{KAPPA_DIR}/greyplot axes nokey o${fnum}_lon_reb accept ");
system ("$ENV{SURF_DIR}/scuover device=xwindows");
system ("$ENV{SURF_DIR}/remsky in=o${fnum}_lon_ext out=o${fnum}_lon_sky");
system ("$ENV{SURF_DIR}/remip in=o${fnum}_lon_sky out=o${fnum}_lon_ip reset accept");
system ("$ENV{SURF_DIR}/intrebin ref=o${fnum}_lon_ip out=o${fnum}_int PIXSIZE_OUT=3.09 SIZE=\[65,65\] REBIN_METHOD=gaussian SCALE=$smooth noguard noloop accept ");

# Set the file name for the rest of the script
$file = "o${fnum}_int";

# Print out the source info & calculate source rotations
source_info();

# Calculate the 4 sets of Stokes vectors
$n = 1;
$i = 1;
while ($n <= 4) {
  calpol($file,$i,$n,$smooth);
  ++$n;
  $i += 4;
}

# Calculate the mean of the q and u vectors
print "\nCalculate the mean of the q, u and i vectors\n";
system ("$ENV{KAPPA_DIR}/maths exp='(ia+ib+ic+id)/4.0' ia=$file\_q1iprot\ ib=$file\_q2iprot\ ic=$file\_q3iprot\ id=$file\_q4iprot\ out=$file\_qav\ ");
print "..q";
system ("$ENV{KAPPA_DIR}/maths exp='(ia+ib+ic+id)/4.0' ia=$file\_u1iprot\ ib=$file\_u2iprot\ ic=$file\_u3iprot id=$file\_u4iprot out=$file\_uav ");
print "..u";
system ("$ENV{KAPPA_DIR}/maths exp='(ia+ib+ic+id)/4.0' ia=$file\_i1\ ib=$file\_i2\ ic=$file\_i3\ id=$file\_i4\ out=$file\_iav\ ");
print "..i\n";

# Calculate the State of Polarisation
print "\nCalculate the polarisation\n";
system ("$ENV{KAPPA_DIR}/maths exp='100*sqrt(ia*ia+ib*ib)' ia=$file\_qav\ ib=$file\_uav\ out=$file\_P\ ");
print "..p";
system ("$ENV{KAPPA_DIR}/maths exp='0.5*atan2d(ia,ib)' ia=$file\_uav\ ib=$file\_qav\ out=$file\_TH\ ");
print "..theta\n";

# Threshold the polarisation values to be no greater than 10%
print "\nThreshold for values outside the range of 0.0% > p > 10%\n";
system ("$ENV{KAPPA_DIR}/thresh in=$file\_P\ out=$file\_Pthr\ thrlo=0.0 newlo=BAD thrhi=10 newhi=BAD ");
system ("$ENV{KAPPA_DIR}/setlabel ndf=$file\_Pthr\ label=Polarisation ");
system ("$ENV{KAPPA_DIR}/setunits ndf=$file\_Pthr\ units=% ");

# Display the results
print "\nDisplaying the results\n";
system ("$ENV{KAPPA_DIR}/display axes clear $file\_iav\ mode=per device=xwindows  ");
system ("$ENV{KAPPA_DIR}/vecplot noclear ndf1=$file\_Pthr\ ndf2=$file\_TH\ device=xwindows veccol=red step=2 vscale=10");

print "\n\n****** E-vectors are plotted ******\n\n";

# Good Housekeeping
print "\n *** Good Housekeeping! ***\n";
foreach (<'o${fnum}_int_q[1-4].sdf'>) {
  unlink ;
  print ".";
}
foreach (<'o${fnum}_int_u[1-4].sdf'>) {
  unlink ;
  print ".";
}
foreach (<'o${fnum}_int_p[1-4].sdf'>) {
  unlink ;
  print ".";
}
foreach (<'o${fnum}_int_th[1-4].sdf'>) {
  unlink ;
  print ".";
}
foreach (<'o${fnum}_int_q[1-4]ip.sdf'>) {
  unlink ;
  print ".";
}
foreach (<'o${fnum}_int_u[1-4]ip.sdf'>) {
  unlink ;
  print ".";
}
foreach (<'o${fnum}_int_p[1-4]ip.sdf'>) {
  unlink ;
  print ".";
}
foreach (<'o${fnum}_int_th[1-4]ip.sdf'>) {
  unlink ;
  print ".";
}

print "\n\n";

exit;

sub source_info {
  # Read in the fits header information
  ($hashref,$status) = fits_read_header("o$fnum");
  die "Nope....couldn't find o$fnum!\n" unless $status == &NDF::SAI__OK;

  # Get the object information
  $object = $$hashref{OBJECT};
  $ra = $$hashref{LONG};
  $dec = $$hashref{LAT};
  $system = $$hashref{CENT_CRD};

  # Calculate the RA & Dec in degrees
  ($h,$m,$s) = split(/:/,$ra);
  $radeg = (15*$h) + (0.25*$m) + (0.00416667*$s);
  ($d,$am,$as) = split(/:/,$dec);
  $decrem = ($am/60)+($as/3600);
  $decrem =~ s/0.//;
  $decdeg = "$d.".$decrem;

  # Calculate the average LST time for each set of 4 w/plate positions
  ($status,@lst) = lst("o$fnum");
  die "Nope....couldn't find o$fnum!\n" unless $status == &NDF::SAI__OK;
  $lst1 = ($lst[0] + $lst[7])/2.0;
  $lst2 = ($lst[8] + $lst[15])/2.0;
  $lst3 = ($lst[16] + $lst[23])/2.0;
  $lst4 = ($lst[24] + $lst[31])/2.0;

  # Calculate the elevation of the source at each of these LST times
  $el1 = sin(19.82583*D2R)*sin($decdeg*D2R)+cos(19.82583*D2R)*cos($decdeg*D2R)*cos(($lst1-$radeg)*D2R);
  $el1 = asin($el1)/D2R;
  $el2 = sin(19.82583*D2R)*sin($decdeg*D2R)+cos(19.82583*D2R)*cos($decdeg*D2R)*cos(($lst2-$radeg)*D2R);
  $el2 = asin($el2)/D2R;
  $el3 = sin(19.82583*D2R)*sin($decdeg*D2R)+cos(19.82583*D2R)*cos($decdeg*D2R)*cos(($lst3-$radeg)*D2R);
  $el3 = asin($el3)/D2R;
  $el4 = sin(19.82583*D2R)*sin($decdeg*D2R)+cos(19.82583*D2R)*cos($decdeg*D2R)*cos(($lst4-$radeg)*D2R);
  $el4 = asin($el4)/D2R;

  # Calculate the source rotations
  $rot1 = srcrot($radeg,$decdeg,$lst1,$el1);
  $rot2 = srcrot($radeg,$decdeg,$lst2,$el2);
  $rot3 = srcrot($radeg,$decdeg,$lst3,$el3);
  $rot4 = srcrot($radeg,$decdeg,$lst4,$el4);

  write;
}

sub srcrot {

  my($r,$d,$l,$e) = @_;
  $r = $r*D2R;
  $d = $d*D2R;
  $l = $l*D2R;
  $e = $e*D2R;

  # Constants
  $lat = 0.34602601;   #Latitude of the JCMT in radians
  $ha = $l-$r;

  $s_rot = cos($lat)*sin($ha)/cos($e);
  $c_rot = (sin($lat)-sin($d)*sin($e))/(cos($d)*cos($e));
  $rot = asin($s_rot);

  if ($s_rot > 0.0) {
    if ($c_rot < 0.0) {
      $rot = PI - asin($s_rot);
    }
  } elsif ($s_rot < 0.0) {
    if ($c_rot < 0.0) {
      $rot = -(PI + asin($s_rot));
    }
  } else {
    if ($c_rot > 0.0) {
      $rot = 0.0;
    } elsif ($c_rot < 0.0) {
      $rot = PI
    }
  }

  $rot = $rot/D2R;

  return $rot
}

format STDOUT =

*************************
   S O U R C E   I N F O

  Object : @>>>>>>>>>>
$object
  RA     : @>>>>>>>>>>
$ra
  DEC    : @>>>>>>>>>>
$dec
  System : @>>>>>>>>>>
$system

  Hour(d): @>>>>>>>>>>
$radeg
  Dec (d): @>>>>>>>>>>
$decdeg

  1 : LST = @<<<<<<< ELEVATION = @<<<<<<< ROTATION = @<<<<<<<
$lst1, $el1, $rot1
  2 : LST = @<<<<<<< ELEVATION = @<<<<<<< ROTATION = @<<<<<<<
$lst2, $el2, $rot2
  3 : LST = @<<<<<<< ELEVATION = @<<<<<<< ROTATION = @<<<<<<<
$lst3, $el3, $rot3
  4 : LST = @<<<<<<< ELEVATION = @<<<<<<< ROTATION = @<<<<<<<
$lst4, $el4, $rot4

*************************

.


sub lst {

my ($status,$xloc,$indf,$comp,@lst,$filename,$el);
$status = &NDF::SAI__OK;

# Read in filename and strip .sdf
$filename = shift;
$filename =~ s/\.sdf//;

$el = undef;

ndf_begin;
ndf_find(&NDF::DAT__ROOT, $filename, $indf, $status);
ndf_xloc($indf, 'SCUCD', 'READ', $xloc, $status);

$comp = 'LST_STRT';
cmp_size($xloc, $comp, $size, $status);
cmp_getvd($xloc, $comp, $size, @lst, $el, $status);

dat_annul($xloc,$status);
ndf_annul($indf, $status);

@lst = map { rad2deg($_); } @lst;

ndf_end($status);

return ($status,@lst);

}

sub calpol {

  my ($file,$i,$n,$smooth) = @_;

  print "\nWorking on Polarimetry set $n\n";

  # Set up variables
  $in1 = "$file".".i".$i;
  $in2 = "$file".".i".++$i;
  $in3 = "$file".".i".++$i;
  $in4 = "$file".".i".++$i;
  $iout = "$file"."_i".$n;
  $qout = "$file"."_q".$n;
  $uout = "$file"."_u".$n;
  $pout = "$file"."_p".$n;
  $thout = "$file"."_th".$n;
  $piout = "$file"."_pi".$n;

  # Deglitch bad pixels
  print "Deglitching bad pixels\n";
  $gout1 = "${file}_glitch1";
  $gout2 = "${file}_glitch2";
  $gout3 = "${file}_glitch3";
  $gout4 = "${file}_glitch4";
  system("$ENV{KAPPA_DIR}/glitch inpic=$in1 outpic=$gout1 where=bad");
  system("$ENV{KAPPA_DIR}/glitch inpic=$in2 outpic=$gout2 where=bad");
  system("$ENV{KAPPA_DIR}/glitch inpic=$in3 outpic=$gout3 where=bad");
  system("$ENV{KAPPA_DIR}/glitch inpic=$in4 outpic=$gout4 where=bad");
  $in1 = $gout1;
  $in2 = $gout2;
  $in3 = $gout3;
  $in4 = $gout4;

  # Calculate the 4 sets of Stokes vectors
  print "\nExtract & calculate the i,q,u vectors and P and Theta\n";
  system ("$ENV{KAPPA_DIR}/calpol in1=$in1 in2=$in2 in3=$in3 in4=$in4 i=$iout q=$qout u=$uout p=$pout theta=$thout ip=$piout debias=false variance=false ");

  # Correct the q and u images for source rotation
  print "Correcting for the source rotation\n";
  system ("$ENV{KAPPA_DIR}/maths exp='0.01*ia*cosd(2*(ib-pa+pb))' ia=$pout ib=$thout pa=$el1 pb=$rot1 out=$qout\iprot\ ");
  system ("$ENV{KAPPA_DIR}/maths exp='0.01*ia*sind(2*(ib-pa+pb))' ia=$pout ib=$thout pa=$el1 pb=$rot1 out=$uout\iprot\ ");


}
