#!/star/bin/ndfperl

# Starlink prolog at end.
# Perl program to add an offset to the bolometer positions
# stored in a file.

# Command line options
use Getopt::Long;


# Use the NDF module
use NDF;

# Use READLINE for I/O
use Term::ReadLine;

# Read the standard options from the command line
$result = GetOptions("help"    => \$h,
                     "version" => \$v,
                     "out=s"   => \$outfile
                     );

$h = 1 unless $result == 1;  # Print help info if unknown option


# Print a help message


($h) && do {
  print qq/
Usage:
  change_nacentre [-h] [-v] infile dx dy
Options:
\t-h[elp]   \tThis message.
\t-v[ersion]\tPrint version number of program.
\tinfile    \tInput file name (no .sdf)
\tdx        \tChange in Nasmyth X direction
\tdy        \tChange in Nasmyth Y direction
Description:
\tShift the Nasmyth centre position of the array.
\tCan be used to take out the difference between the
\tcentres of the LONG and SHORT wave arrays.
\tThis command is designed to be run after EXTINCTION
\tcorrection.
/;
  exit;
};

($v) && do {
  print qq/change_nacent: version 0.10 (1998)
/;
  exit;
};

# Create a Term object
$term = new Term::ReadLine 'nacent';


# Now read the command line arguments one at a time.

if ($#ARGV > -1) {
  $in = shift;
} else {
  $prompt = "Please enter input file name: ";
  $in = $term->readline($prompt);
}
$in =~ s/\.sdf$//;  # Strip trailing sdf


if ($#ARGV > -1) {
  $du3 = shift;
} else {
  $prompt = "Please enter NA shift in X direction: ";
  $du3 = $term->readline($prompt);
}

if ($#ARGV > -1) {
  $du4 = shift;
} else {
  $prompt = "Please enter NA shift in Y direction: ";
  $du4 = $term->readline($prompt);
}


# Now we have all the required parameters

# Start NDF
ndf_begin;
$status = &NDF::SAI__OK;

# Open the file
ndf_open(&NDF::DAT__ROOT, $in, 'UPDATE', 'OLD', $indf, $place, $status);

# Find the SCUBA extension
ndf_xloc($indf, 'SCUBA', 'UPDATE', $xloc, $status);

# Read the BOL_DU3 extension if dx is non zero
# Can simply add this value to all members of the BOL_DU* array
# even if the bolometers arent being used


foreach $coord ('du3', 'du4') {


  $du = $$coord;
  $comp = 'BOL_'.uc($coord);

  print "Adding $du arcsec to $coord\n";

  if ($du != 0.0) {
    # Read
    cmp_getvd($xloc, $comp, 200, @offsets, $el, $status);

    # Add offset
    map { $_ += $du } @offsets;

    # Write
    cmp_putvr($xloc, $comp, $el, @offsets, $status);

  }
}

# Annul locator
dat_annul($xloc, $status);

# Close NDF
ndf_annul($indf, $status);

# Shut down ndf
ndf_end($status);

__END__

*+
*  Name:
*    CHANGE_NACENTRE

*  Purpose:
*    Shift the Nasmyth centre of the array

*  Language:
*    Perl 5

*  Description:
*    This routine shifts the position of the Nasmyth centre of a
*    SCUBA array. It can be used to take out the small difference
*    between the centres of the LONG and SHORT wave arrays.
*    Should be run after EXTINCTION.

*  Usage:
*    change_nacentre [-h | -v ] infile dx dy

*  ADAM Parameters:
*    -h
*      Return a help message only.
*    -v
*      Return the version number of scunoise
*    infile
*      Input file name. The file is modified in place.
*    dx
*      Shift in Nasmyth X (du3) direction
*    dy
*      Shift in Nasmyth Y (du4) direction

*  Examples:
*    change_nacentre
*      Will prompt for input file name and shift
*    change_nacentre file 5 -3
*      Will move the array centre of file.sdf by (5,-3) arcsec.

*  Notes:
*    This command can only be reversed by running change_nacentre
*    with minus the previous X,Y shift.
*    EXTINCTION must have been run on the input file (otherwise the
*    file will contain more than 1 array) -- this is not checked
*    for explicitly.

*  Authors:
*    Tim Jenness (JAC)

*  Copyright:
*     Copyright (C) 1995,1996,1997,1998,1999 Particle Physics and Astronomy
*     Research Council. All Rights Reserved.

*  Related Applications:
*    SURF: REBIN

*-
