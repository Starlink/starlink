#!/bin/perl

# scan.pl
#
#   Script to automate scan/map data reduction.
#   Design it such that it can be used in ORACDR

# Command line options

use Getopt::Long;


# Use ORAC modules
use lib "/jcmt_sw/lib/perl5";

use ORAC::Frame::JCMT;
use ORAC::Group::JCMT;
use NDF;                                # to access NDF headers
use ORAC::Msg::ADAM::Control;           # messaging control for ADAM
use ORAC::Msg::ADAM::Task;              # monolith control for ADAM
use ORAC::Msg::ADAM::Shell;             # monolith control via shell

# Unbuffered
$| = 1;

# Options

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
  remdbm [-h] [-v] [-out=s] files
Options:
\t-h[elp]   \tThis message.
\t-v[ersion]\tPrint version number of program.
\t-out=s    \tSpecify output filename. Default is "final".
\t"files"   \tList of files to be processed.
Description:
\tThis program should be used to reduce SCAN-MAP data taken
\tusing the technique described by Emerson 
\t(1995, ASP Conf Ser 75, 309). The following is assumed by the
\tprogram:
\t\t1. Each chop configuration has been regridded independently
\t\t   using the SURF task REBIN. (so that the FITS header
\t\t   can be read).
\t\t2. Each image must have identical dimensions and pixel size
\t\t   (they need not be square or a power of 2)
\t\t3. The images must be rebinned in the same coordinate
\t\t   as the chop throw. (RJ for RB and GA, PL for moving objects)
\tCurrently compliance to the above is only assumed.
/;
  exit;
};

# Print version number

($v) && do {
  print qq/remdbm: version 0.99 (1998)
/;
  exit;
};

# Check for Starlink
die "Must execute a Starlink login" unless (exists $ENV{SURF_DIR});  


# Setup default output filename

if ($outfile =~ /./) {
  $outfile =~ s/\.sdf$//;  # Strip .sdf
  $output = $outfile;
} else {
  $output = "final";
}


# Deal with ADAM interface

print "Starting monoliths...";
$Mon{surf_mon} = new ORAC::Msg::ADAM::Task("surf_mon_$$", $ENV{SURF_DIR}."/surf_mon");
$Mon{kappa_mon} = new ORAC::Msg::ADAM::Task("kappa_mon_$$",$ENV{KAPPA_DIR}."/kappa_mon");

#$Mon{surf_mon} = new ORAC::Msg::ADAM::Shell("surf_mon_$$", "/jcmt_sw/scuba/optimize/redsdir/surf_mon");
#$Mon{kappa_mon} = new ORAC::Msg::ADAM::Shell("kappa_mon_$$",$ENV{KAPPA_DIR}."/kappa_mon");

$adam = new ORAC::Msg::ADAM::Control;
$adam->init;
$adam->timeout(120);     # task timeout


$Mon{kappa_mon}->contactw || die "Couldnt contact monoliths";
print "Done\n";

# Read the file list

@files = @ARGV;

# Setup a new Group

$Grp = new ORAC::Group::JCMT("junk");


foreach $file (@files) {

  # Set up Frames
  my $Frm = new ORAC::Frame::JCMT($file);

  # And add to group so long as we are dealing with
  $Grp->push($Frm);

}

die "No files specified" if $Grp->num == -1;

# Check that they are all raster maps



#### Now we are in the same context as ORACDR


# First stab -- assume that there is only one chop config per file.
# Also assume that all files are same size with same pixel scale

# Select first map
$Frm = $Grp->frame(0);

# Find dimensions of map
ndf_begin;
$status = &NDF::SAI__OK;
ndf_find(&NDF::DAT__ROOT, $Frm->file, $indf, $status);
@dim = ();
ndf_dim($indf, 2, @dim, $ndim, $status);
ndf_end($status);

die "Should be 2-dimensional data" if $ndim != 2;

# Store pixel size
$pixsize = $Frm->hdr('SCUPIXSZ');


# Generate the weight

@ffts    = ();   # Store FFT names
$wt = "weight";  # Name of file containing total weight
$ext = ".sdf";   # Filename extension
$i = 0;          # Counter
$im = "im";      # Running imaginary component
$re = "re";      # Real component so far

foreach $frame ($Grp->members) {
  $i++;  # increment counter
  print "Loop number $i\n";

#  $Mon{kappa_mon}->resetpars;  # Reset parameters each time round

  $chop_pa = $frame->hdr('CHOP_PA');
  $chop_thr = $frame->hdr('CHOP_THR');

  print "Chop: PA=$chop_pa THROW=$chop_thr\n";

  $outwt = 'wt_' . $chop_pa . '_' . $chop_thr;
  $outft = 'ft_' . $chop_pa . '_' . $chop_thr;

  $string = "chop=$chop_thr pa=$chop_pa pixsize=$pixsize size=[$dim[0],$dim[1]] ftchop=$outft wtchop=$outwt accept";

  $status = $Mon{surf_mon}->obeyw("scumakewt","$string");
  check_status($status);

  # Calculate the total weight (so that we can remove the files
  # when they are no longer needed

  # if this is first time round then simply rename weight to weight
  # else add to weight and rename

  if ($i > 1) {
    # Add to previous
    $status = $Mon{kappa_mon}->obeyw("add","in1=$outwt in2=$wt out=junk title='Total_Weight' reset ");
    rename ("junk$ext", "$wt$ext");
    # Remove the weight
    unlink($outwt . $ext) or die "Eek $outwt$ext $!"; 

  } else {
    rename ("$outwt$ext", "$wt$ext");
  }

  # Fourier transform the input data multiplying the DEC (chop_pa=0) by -1
  # This probably implies that I have got the weights inverted for
  # this chop position angle.

  if ($chop_pa == 0) {

    $status = $Mon{kappa_mon}->obeyw("cmult","in=".$frame->file." scalar=-1.0 out=junk reset");
    check_status($status);
    $string = "in=junk";
  } else {
    $string = "in=".$frame->file;
  }

  $realout = "re_" . $chop_pa . '_' . $chop_thr;
  $imout = "im_" . $chop_pa . '_' . $chop_thr;

  $status = $Mon{kappa_mon}->obeyw("fourier","$string realout=$realout imagout=$imout hermout=! reset");
  check_status($status);

  # Remove junk.sdf
  unlink "junk$ext" if (-e "junk$ext");

  # We now need to divide this by the FT of the chop and multiply
  # by the weight of the chop 
  # Call the FT of the chop iF, then we have (chop is purely imaginary)
  # 
  # New = (x + iy) * F**2 / iF
  #     = (x + iy) F / i
  #     = ( x + iy ) F *  i 
  #         ------       ---
  #           i           i
  #
  #     = F ( y - ix)

  # This means we multiply the imaginary part by the FT of the chop
  # to generate the new real component, and the real by -F to generate
  # the new imaginary component
  
  $status = $Mon{kappa_mon}->obeyw("mult","in1=$realout in2=$outft out=ijunk reset");
  check_status($status);
  $status = $Mon{kappa_mon}->obeyw("cmult","in=ijunk scalar=-1.0 out=imtemp reset");
  check_status($status);
  $status = $Mon{kappa_mon}->obeyw("mult","in1=$imout in2=$outft out=retemp reset");
  check_status($status);

  # Remove the intermediate files (FFTs)
  unlink "$outft$ext", "$realout$ext", "$imout$ext", "ijunk$ext";

  # If we have been round before coadd to the running total
  if ($i > 1) {

    $status = $Mon{kappa_mon}->obeyw("add","in1=imtemp in2=$im out=junk reset");

    check_status($status);
    rename ("junk$ext", "$im$ext");

    $status = $Mon{kappa_mon}->obeyw("add","in1=retemp in2=$re out=junk reset");
    check_status($status);
    rename ("junk$ext", "$re$ext");
    unlink "imtemp$ext", "retemp$ext";

  } else {
    # else rename files
    rename ("retemp$ext", "$re$ext");
    rename ("imtemp$ext", "$im$ext");
  }


}


# Now we have the total weight, and the processed real and imaginary
# components.

# Divide real and imaginary by the total weight

$status = $Mon{kappa_mon}->obeyw("div","in1=$re in2=$wt out=rediv reset");
check_status($status);
$status = $Mon{kappa_mon}->obeyw("div","in1=$im in2=$wt out=imdiv reset");
check_status($status);

# Remove the weight
unlink "$wt$ext", "$re$ext", "$im$ext";

# Inverse fourier
$status = $Mon{kappa_mon}->obeyw("fourier","inverse realin=rediv imagin=imdiv out=$output reset");
check_status($status);

# Remove the final junk files
unlink "rediv$ext", "imdiv$ext";

print "Result stored in $output\n";


exit;

# Status check

sub check_status {
  my $status = shift;

  die "Bad status: $status" if $status != 0;


}
