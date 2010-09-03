#!/bin/perl

# Script to ease data reduction of polarimetry data

# Reads in the output from SCUPHOT.
# Then for each bol_peak finds the stats of each
# set of integrations (generally groups of 10).
# Have to do it in groups since a single data file contains
# a number of different wave plate positions.
#

#  Authors:
#     Tim Jenness (JAC)

#  Copyright:
#     Copyright (C) 1998,1999 Particle Physics and Astronomy
#     Research Council. All Rights Reserved.



# Needs READLINE and NDF

use NDF;
use Getopt::Long;
use Term::ReadLine;

#
# command line arguments
#

my $status = GetOptions("sub=s","h","out=s");

($opt_h) && do {
print qq{
pol [-h] [-s sub] [-out outfile] (file|number) integrations/waveplate

extract polarimetry information from photometry file

 Args:
   -h: help
   -s: sub-instrument (default 'lon')
       This is the string that goes between  oNN_ and _pht
       in the standard SURF naming convention
       This option is ignored if a full filename is provided
   -out: output filename (default is obsNN.dat)
   file|number: The filename to be processed or the observation number
       If a number is provided the filename is assumed to be oNN_SUB_pht.sdf
   int/wplate - number of integrations per waveplate move (default is 1)

};
exit;
};

# Get the location of KAPPA

if (defined $ENV{"KAPPA_DIR"}) {
  $kappa = $ENV{"KAPPA_DIR"};
} else {
  die "Can not start - The KAPPA_DIR environment variable is not set\n";
}

# Need to know input filename and number of integrations per
# waveplate position


# Set up READLINE

$term = new Term::ReadLine 'perl';

# Read in the HDS name from the command line
#  - if not there then ask for it

$file = shift;

unless (defined $file) {
  $prompt = "Please enter input file name: ";
  $file = $term->readline($prompt);
}

# Drop the .sdf if necessary

$file =~ s/\.sdf//;

# If file matches a number then assume we really mean
# oNN_lon_pht

if ($file =~ /^\d+/) {

  # Need to check $sub
  unless (defined $opt_sub) {
    $sub = "lon";
    print "using default sub-instrument: lon\n";
  } else {
    $sub = $opt_sub;
  }

  $run_no = $file; # This is the run number

  $file = "o${file}_${sub}_pht";
}

print "Reading from file $file\n";

# Read in the integrations per wave plate

$int_per_wp = shift;

unless (defined $int_per_wp) {
  $int_per_wp = 4;
}
print "Using $int_per_wp integrations per waveplate\n";

# Find out all the _peak NDFs in the input file

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
  push(@names, $name) if $name =~ /_PEAK$/;
}

dat_annul($loc, $status);

die "Error opening $file\n" unless $status == &NDF::SAI__OK;

# Now we are ready to roll.

die "This is not a photometry output file\n" if $#names == -1;

# Start up NDF
ndf_begin;


# Loop over each name
foreach $name (@names) {

  # Read the header info
  ($hashref,$status) = fits_read_header("$file.$name");

  # If $run_no is undefined we need to read it from the fits header
  $run_no = $hashref->{RUN} unless (defined $run_no);

  # Bolometer name
  $name =~ /(.*)_PEAK/ & ($bol = $1);

  # Get the root output name
  if (defined $out) {
    $root = $out;
  } else {
    $root = "obs${run_no}";
  }

  # Construct output file name if necessary
  # Use the bolometer name as well
  # and open the file. Treat $out as a root name.

  $output_file = $root . "$bol"  . ".dat";

  print "Output filename is $output_file\n";

  open(OUT, "> $output_file") || die "Error could not open output file: $!";

  # print the first line of the header
  print OUT "$$hashref{OBJECT} $$hashref{WAVE_1} ${bol}_$$hashref{RUN} $$hashref{UTDATE}\n";

  # Now need to find out how many integrations we have so that
  # we can split it into sections

  ndf_find(&NDF::DAT__ROOT, "$file.$name", $indf, $status);

  #    ndf_size($indf, $npix, $status);
  @dim = ();
  ndf_dim($indf, 1, @dim, $ndim, $status);
  #    ndf_size($indf, $npix, $status);
  $npix = $dim[0];
  ndf_annul($indf, $status);

  # Now that I know the size print the second header line
  $nposplate = int($npix/$int_per_wp + 0.99);
  $ncycle = int($nposplate/16.0);

  # Convert the time to hours
  (@times) = split(/:/,$$hashref{STSTART});
  $lst = $times[0] + ($times[1] / 60) + ($times[2] / 3600);

  print OUT "$nposplate $ncycle $$hashref{MEANRA} $$hashref{MEANDEC} $lst\n";

  # Number of time round the loop depends on $npix

  for ($i = 1; $i <= $npix; $i += $int_per_wp) {
    $start = $i;
    $end = $start + $int_per_wp - 1;
    $end = "" if $end > $npix;

    # Find stats of section (using KAPPA for now)
    $section = "$file.$name($start:$end)";
    $exstat = system("$kappa/stats '$section' > /dev/null");

    ($mean) =  par_get("mean", "stats", \$status);
    ($sigma) = par_get("sigma", "stats", \$status);

    print OUT "$mean   $sigma\n";

  }

  # Close output file
  close(OUT);

}


# Shut down NDF

ndf_end($status);
