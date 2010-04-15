#!/bin/perl -s

# Script to calculate the flatfield by analysing the output of BOLREBIN

#  Authors:
#     Tim Jenness (JAC)

#  Copyright:
#     Copyright (C) 1998 Particle Physics and Astronomy
#     Research Council. All Rights Reserved.


use PDL;
use PDL::JAC::Timj qw/:Func/;
use PDL::IO::NDF qw/:Func/;
use PDL::Graphics::PGPLOT;
use NDF;

use Term::ReadLine;

# Deal with the simple command line arguments:

($h) && do {
  print qq/
  Usage: calcflat  ndf radius
  Arguments:
    -h  \t Help
    ndf \t The HDS container to be analysed (output of BOLREBIN)
    radius\t Radius of the aperture in pixels

/;
  exit;
};





# Start by setting up a graphics device

dev "/xserve";
ctab fire;

#$file = 'n87_bols';
$refimage = 'H7';
#$radius = 30;  # 30 pixels radius
#$radius = 10; # 3 arcsec pixels

# Set up readline

$term = new Term::ReadLine 'perl';

# Read in the HDS name from the command line
#  - if not there then ask for it

$file = shift;

unless (defined $file) {
  $prompt = "Please enter input file name: ";
  $file = $term->readline($prompt);
}


# First of all do a trace on the input file in order to find the name
# of all the NDFs

$status = &NDF::SAI__OK;
hds_open($file, 'READ', $loc, $status);

die "Unable to open $file\n" unless $status == &NDF::SAI__OK;

dat_ncomp($loc, $ncomp, $status);

@names = ();
for $comp (1..$ncomp) {
  dat_index($loc, $comp, $nloc, $status);
  dat_name($nloc, $name, $status);
  dat_annul($nloc, $status);
  push(@names, $name);
}

dat_annul($loc, $status);

die "Error opening $file\n" unless $status == &NDF::SAI__OK;

# Pixel radius

$radius = shift;

unless (defined $radius) {
  $prompt = "Please enter radius of aperture (pixels): ";
  $radius = $term->readline($prompt);
}


# Now loop over members

$sum = {};
foreach $ndf (@names) {

  $a = rndf("$file.$ndf");

  imag $a,0;

  # Find max
  max2d_ind($a, $max=null, $xpos=null, $ypos=null);

  # Get mask
  $b = rvals(zeroes(dims($a)),$xpos, $ypos);

  $a *= ($b < $radius);

  # Get rid of bad pixels
  $a *= ($a > -1.0e10);

  #imag $a,0;

  $sum{$ndf} = sum($a);
}

# Now work out the Relative value

foreach $ndf (@names) {
   print "$ndf: ",$sum{$refimage} / $sum{$ndf},"\n";

}

