#!/bin/perl

# Script to ease data reduction of polarimetry data

# Reads in the output from SCUPHOT.
# Then for each bol_peak finds the stats of each
# set of integrations (generally groups of 10).
# Have to do it in groups since a single data file contains
# a number of different wave plate positions.
# 

# Needs READLINE and NDF

use NDF;
use Term::ReadLine;



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

# Read in the integrations per wave plate

$int_per_wp = shift;

unless (defined $int_per_wp) {
  $prompt = "How many integrations per waveplate position: ";
  $int_per_wp = $term->readline($prompt);
}


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

  $name =~ /(.*)_PEAK/ & ($bol = $1);
  print "Bolometer: $bol\n";

  print "Section   Mean   Sigma\n";

    # Now need to find out how many integrations we have so that
    # we can split it into sections

    ndf_find(&NDF::DAT__ROOT, "$file.$name", $indf, $status);

#    ndf_size($indf, $npix, $status);
    @dim = ();
    ndf_dim($indf, 1, @dim, $ndim, $status);
#    ndf_size($indf, $npix, $status);
    $npix = $dim[0];
    print "Npix = $npis\n";
    ndf_annul($indf, $status);

    # Number of time round the loop depends on $npix

    for ($i = 1; $i <= $npix; $i += $int_per_wp) {
      $start = $i;
      $end = $start + $int_per_wp - 1;
      $end = "" if $end > $npix;

      # Find stats of section (using Kappa for now)
      $section = "$file.$name($start:$end)";
      $exstat = system("$kappa/stats '$section' > /dev/null");

      ($mean) =  par_get("mean", "stats", \$status);
      ($sigma) = par_get("sigma", "stats", \$status);

      print "($start:$end)    $mean   $sigma\n";

    }



}

# Shut down NDF

ndf_end($status);
