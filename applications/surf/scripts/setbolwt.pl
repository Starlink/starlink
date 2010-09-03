#!/star/bin/ndfperl

# Program to set the bolometer weights:
#
#  - Simple way:
#     1. Read in NDF name
#     2. Calculate stats for each bolometer using KAPPA STATS
#     3. Calculate weights relative to Central pixel
#     4. Write weights to NDF extension

# If a weights file is specfied the weights are set for each
# input ndf using this one file (assuming the number of bolometers matches).



$| = 1;

# Load the NDF module
use NDF;

# Command line options
use Getopt::Long;

use Term::ReadLine;


# Get the location of KAPPA

if (defined $ENV{"KAPPA_DIR"}) {
  $kappa = $ENV{"KAPPA_DIR"};
} else {
  die "Can not start - The KAPPA_DIR environment variable is not set\n";
}


# Read command line options

$result = 0;
$result = GetOptions("help" => \$h,
		     "filelist=s" => \$flist,
		     "wtfile=s" => \$wtfile);


($h) && &print_help_message;


# Global status
$status = &NDF::SAI__OK;
$good = &NDF::SAI__OK;


# Now need to get a list of files or a filename containing a list of files.
# Format of the text file is same as that used for REBIN and DESPIKE

# Option 1 is to check for the filelist option

@ndfs = ();

if ($flist =~ /./) {

  if (-e $flist) {

    @ndfs = parse_input_file($flist);

  } else {
    print "Couldn't open $flist. Ignoring this option.\n";
  }
}

# Option 2 is to read the remaining arguments from the command line

push(@ndfs, @ARGV);


# Read the command line
# Simple case: Read in name of NDF.
#  Check that file is okay.

# If we have done all that and still don't have any input
# we ask for a filename - currently I am not going to support
# the input of a text file at this point.

if ($#ndfs == -1) {
  $term = new Term::ReadLine 'bolwt';
  $prompt = "Please enter names of input NDFs: ";
  my $ndf = $term->readline($prompt);
  push(@ndfs, $ndf);
}


# Setup ndf environment
err_begin($status);
ndf_begin;


# Loop over all NDFs to determine bolometer number and whether
# A particular NDF can be opened or not

foreach $ndf (@ndfs) {

  # Check status each time around
  last unless $status == $good;

  # Remove trailing sdf
  $ndf =~ s/\.sdf$//;

  # Check that files exists simply by doing an ndf_find
  # and checking for good status.

  ndf_find(&NDF::DAT__ROOT, $ndf, $indf, $status);

  if ($status == $good) {
    print "Using the NDF $ndf...\n";

  } else {

    print "Error opening $ndf. Ignoring.\n";
    err_annul($status);
    next;
  }

# Find the shape of the NDF (To calculate the number of bolometers)

  @dim = ();
  $ndim = 0;
  ndf_dim($indf, 3, @dim, $ndim, $status);

# Check the HISTORY to make sure that we have run EXTINCTION
# Have to do this to make sure we don't compare LONG and SHORT wave
# bolometers. (Doesn't stop us comparing multiple files with different
# arrays though)

  if ($status == $good) {

    $okay = 0;
    ndf_hnrec($indf, $nrec, $status);

    for ($i=1; $i <= $nrec; $i++) {
      ndf_hinfo($indf, 'APPLICATION', $i, $app, $status);

      if ($app =~ /^EXTINCTION/) {
	$okay = 1;
	last;
      }
    }

    if ($status == $good) {
      unless ($okay) {
	$status = &NDF::SAI__ERROR;
	err_rep(' ',"$ndf has not been processed through EXTINCTION", $status);
      }
    }
  }

  # Only continue if status is good

  if ($status == $good) {

    print "\tDimensions: ". join(" x ",@dim) . "\n";

    $nbol = $dim[0];
    print "\tNumber of bolometers: $nbol\n";

    # Everything is okay so store the number of bolometers
    # as an array of anonymous hashes
    $obj = {};
    $$obj{NBOLS} = $nbol;
    $$obj{NAME} = $ndf;

    push(@good, $obj);

  } else {
    # Loop round again
    err_rep(' ',"Error during initial processing of $ndf - removing from list",
	    $status);
    err_flush($status);
    err_annul($status);
  }

  # Close the file and shutdown NDF
  ndf_annul($indf, $status);

}

# Shut down the NDF system
ndf_end($status);


# Stop if we have no files after all this
if ($#good == -1) {die "No good NDF files specified";}



# If a weight file has been specified we simply use that
# Else we calculate the weights from the bolometer statistics

if ($wtfile =~ /./) {

  open(WTFILE, $wtfile) || die "Couldnt open weights file:$!";

  my @weights = ();
  foreach $line (<WTFILE>) {
    chomp($line);
    push(@weights, $line) if $line eq $line * 1.0;
  }
  close(WTFILE);

  # Store the weights
  foreach $ref (@good) {
    $$ref{WEIGHTS} = \@weights;
  }

} else {

# Now we need to calculate statistics for each bolometer
# Starlink status drives me mad sometimes

  # Loop over files
  foreach $ref (@good) {

    last unless $status == $good;

    my $ndf = $$ref{NAME};
    my $nbol = $$ref{NBOLS};

    print "Processing NDF: $ndf ($nbol bolometers)\n";

    my @stats = ();
    foreach (1..$nbol) {

      last if $status != $good;
      print "\tCalculating stats for bolometer $_/$nbol : ";
      $exstat = system("$kappa/stats \'$ndf($_,)\' > /dev/null");

      if ($exstat != 0) {
	$status = &NDF::SAI__ERROR;
	err_rep(' ','Error from STATS command', $status);
	print "\n";  # To tidy up the printing
      }

      ($sigma) = par_get("sigma", "stats", \$status);

      push(@stats, $sigma);
      print "$sigma\n" unless $status != $good;

    }

    # Store the statistics
    $$ref{STATS} = \@stats;

  }

  print "\n";

  # Now have an array of statistics
  # Calculate the weights

  if ($status == $good) {

    # Get info from reference pixel
    $refpix =  int(($nbol)/2) + 1;

    # EEk!
    $refval = ${${$good[0]}{STATS}}[$refpix-1];

    print "Calculating weights relative to bolometer $refpix of ".
      ${$good[0]}{NAME} . "\n";



    foreach $ref (@good) {

      my @weights = ();

      foreach $val (@{$$ref{STATS}}) {

	# First check for bad values
	if ($val <= 0) {

	  $weight = 0.0;

	} else {

	  $weight = ($refval / $val)**2;
	}
	push(@weights, $weight);

      }

      $$ref{WEIGHTS} = \@weights;
    }
  }
}

# Exit if status is bad
if ($status != $good) {
  err_flush($status);
  err_end($status);
  die "Error occurred somewhere";
}

# Now need to write the weights out to each of the input files

ndf_begin;
print "\n";

foreach $ref (@good) {

  my $ndf = $$ref{NAME};
  my $nbol = $$ref{NBOLS};
  my @weights = @{$$ref{WEIGHTS}};

# Check the size of the weights array
  if ($#weights != $nbol-1) {
    my $total = $#weights + 1;

    # This is painful -- using msg_out to print. Too lazy to do the
    # word wrapping any other way!
    msg_out(' ',"Number of weights ($total) does not match the number of bolometers stored in the file ($nbol) for file $ndf. Skipping.\n", $status);
    next;
  }


  print "Setting weights to file $ndf\n";

  # Now need to open the file again and write the weights
  $place = 0;
  ndf_open(&NDF::DAT__ROOT, $ndf, 'UPDATE', 'OLD', $indf, $place, $status);

  if ($status == $good) {
    ndf_xloc($indf, 'REDS', 'UPDATE', $xloc, $status);

    # If status is bad means that there is no REDS extension
    # Fix that by adding one
    if ($status != $good) {
      err_annul($status);
      @dummy = ();
      ndf_xnew($indf, 'REDS', 'SURF_EXTENSION', 0, @dummy, $xloc, $status);
    }

    # Erase the BOLWT array if necessary
    dat_there($xloc, 'BOLWT', $reply, $status);
    dat_erase($xloc, 'BOLWT', $status) if $reply;

  # create the array structure
    @vals = ($nbol);
    cmp_mod($xloc, 'BOLWT', '_REAL', 1, @vals, $status);
    cmp_put1r($xloc, 'BOLWT', $nbol, @weights, $status);


  }

# Tidy up
  dat_annul($xloc, $status);
  ndf_annul($indf, $status);

  if ($status != $good) {
    err_rep(' ',"Error setting weights for file $ndf", $status);
    err_flush($status);
  }

}

ndf_end($status);

# Shut down error
err_end($status);




# Subroutines

# Routine to parse the REBIN-style input file
#
#  Input file is ASCII of format:
#
#   FILE WEIGHT SHIFT_DX SHIFT_DY
#  Comment characters are allowed (anything after a '#' is a comment).
#  Blank lines are allowed

sub parse_input_file {

  my $file = shift;

  open(FILE, $file) or die "Couldnt open text file: $!";

  my @files = ();
  foreach my $line (<FILE>) {

    # First need to strip off any comment fields
    $line =~ s/\#.*//;

    # Strip off leading spaces
    $line =~ s/^\s+//;

    # Skip if there is nothing on the line
    next if $line !~ /./;

    $file = (split(/\s+/, $line))[0];

    # Just in case we should check for 'SCUBA sections'
    $file = (split(/\{/, $file))[0];

    # Assume that the file exists
    push(@files, $file);

  }
  close(FILE);

  return @files;
}





# This prints the help message
sub print_help_message {


  print qq/
Description:
  This routine sets the bolometer weights.
  It can do this in two ways:

  1. Calculate the statistics for each bolometer then generate the
     weights relative to the central pixel. Should not be used when
     a strong source is present.

  2. Read the weights from a text file using the -wtfile option.

  Writes to the BOLWT extension. This extension is then read by REBIN.

  Multiple files can be referenced to the first file by specifying
  multiple files on the command line or by using a REBIN-style input
  file and the -filelist option. In conjunction with the -wtfile option
  all input files are given the same weights.

Usage:
  setbolwt [-h] [-wtfile=] [-filelist=] filename <filename2> <filename3>
Options:
   -h[elp]\t This message
   -filelist=s\t An ASCII text file containing a list of files
   -wtfile=s\t An ASCII text file containing the weights

  The input filename must have been extinction corrected (since currently
  the EXTINCTION task will not know that it should split the BOLWT
  extension).

  Multiple filenames can be read from the command line
   e.g. *_lon_ext.sdf

  If the filelist option is used the input file must be of the
  same format as allowed for REBIN and DESPIKE (ie can in fact just
  be a list of input files (one file per line) but can include comments
  and map shifts)

  When multiple files are read in all the bolometer weights are calculated
  relative to the central pixel of the first map.

  Currently no check is performed to make sure that bolometers from
  the same array are being compared.
/;

exit;


}


__END__


*+
*  Name:
*    SETBOLWT

*  Purpose:
*    Calculate or set bolometer weights

*  Language:
*    Perl 5

*  Description:
*    This routine sets the bolometer weights.
*    It can do this in two ways:
*
*     1. Calculate the statistics for each bolometer then generate the
*        weights relative to the central pixel. Should not be used when
*        a strong source is present. The weights are calculated by
*        using KAPPA STATS to calculate the standard deviation of
*        each bolometer in turn. The weight is defined as the relative
*        variance between this bolometer and the reference bolometer.
*
*     2. Read the weights from a text file using the -wtfile option.
*
*    Writes to the BOLWT extension. This extension is then read by REBIN.
*
*    Multiple files can be referenced to the first file by specifying
*    multiple files on the command line or by using a REBIN-style input
*    file and the -filelist option. In conjunction with the -wtfile option
*    all input files are given the same weights.

*  Usage:
*    setbolwt [-h] [-wtfile=] [-filelist=] filenames...

*  ADAM Parameters:
*    -h
*      Return a usage message.
*    -wtfile=file
*      An ASCII text file containing the weights, one weight per line
*      corresponding to the order of bolometers stored in the file.
*    -filelist=file
*      An ASCII text file containing a list of files to be processed.
*      There must be one file per line and it must be in a form
*      acceptable to REBIN (ie comments can be included).
*    filenames
*      List of filenames to be processed. Wild cards can be used.
*      eg *_lon_ext.sdf.

*  Examples:
*    setbolwt
*      The user will be prompted for a list of input NDFs. The weights
*      will be calculated by setbolwt.
*    setbolwt -wtfile=weights.dat file1
*      Set the weights in file1.sdf from the list contained in
*      weights.dat
*    setbolwt file1 file2 file3 file4
*      Calculate the weights of each bolometer in all four files
*      relative to the central pixel in file1.sdf.
*    setbolwt -wtfile=wt.dat -filelist=rebin.inp
*      Set the weights of the files listed in rebin.inp to those
*      stored in wt.dat (same weights for each file).

*  Authors:
*    Tim Jenness (JAC)

*  Copyright:
*     Copyright (C) 1995,1996,1997,1998,1999 Particle Physics and Astronomy
*     Research Council. All Rights Reserved.



*  Notes:
*    - Input files must have been extinction corrected so that only
*      one sub-instrument is present per file.
*    - When multiple files are used bolometers are compared to the
*      central bolometer of the first file.
*    - If source signal is present in any bolometer at a level
*      significantly above the noise, the automatic weighting will
*      be skewed (in fact the bolometer with the source signal will
*      be down-weighted relative to all the others since the standard
*      deviation on the bolometer will be much higher.). The weights
*      must be set via an external file in this case.

*  Related Applications:
*    SURF: REBIN
*    KAPPA: STATS

