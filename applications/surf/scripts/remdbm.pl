#!/star/bin/ndfperl

# remdbm.pl
#
#   Script to automate scan/map data reduction.
#   Design it such that it can be used in ORACDR
use sigtrap qw/die normal-signals error-signals/;
use Carp;

use Getopt::Long;                       # Command line options
use NDF;                                # to access NDF headers
use Starlink::Versions qw/ starversion_gt /;

use constant DIAMETER => 15.0;          # Diameter of telescope

$use_ams = 0;   # We are not using AMS

# See if perl/AMS is available
eval 'use Starlink::AMS::Init';
if (! $@) {
  $use_ams = 1;

  # Load in the rest of the modules
  eval 'use Starlink::ADAM ()';
  die "Error loading Starlink messaging:\n $@" if $@;
  eval 'use Starlink::AMS::Task';
  die "Error loading Starlink messaging:\n $@" if $@;

  # Cant put this inside the packages since they
  # do not seem to be evaluated.
  $SAI__OK = &Starlink::ADAM::SAI__OK;
  $DTASK__ACTCOMPLETE = &Starlink::ADAM::DTASK__ACTCOMPLETE;
}

# Need to uncomment if we are using external ORAC classes
#use ORAC::Msg::ADAM::Shell;             # monolith control via shell
#use ORAC::Frame::JCMT;
#use ORAC::Group::JCMT;
# Would like to make it such that it sees whether
# ADAM messaging is available and uses it if it is.
#use ORAC::Msg::ADAM::Control;           # messaging control for ADAM
#use ORAC::Msg::ADAM::Task;              # monolith control for ADAM



# Unbuffered
$| = 1;

# Options

# Read the standard options from the command line
$result = GetOptions("help"    => \$h,
		     "version" => \$v,
		     "out=s"   => \$outfile,
                     "noams"   => \$noams,
		     "filter"  => \$filter,
		     );

$h = 1 unless $result == 1;  # Print help info if unknown option

# Allow for Override AMS flag (only relevant if $use_ams is already true.)
$use_ams = 0 if $noams;

# Print a help message

($h) && do {
  print qq/
Usage:
  remdbm [-h] [-v] [-out=s] [-noams] files
Options:
\t-h[elp]   \tThis message.
\t-v[ersion]\tPrint version number of program.
\t-out=s    \tSpecify output filename. Default is "final".
\t-noams    \tTurn off ADAM messaging (default is false if ADAM not found)
\t-filter   \tTurn on high frequency filtering
\t          \tmessaging is allowed)
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
  print "remdbm: version 0.99 (1998)\nStarlink Version: ";
  if ($use_ams) {
    print "AMS communication enabled\n";
  } else {
    print "AMS communication disabled\n";
  }
  exit;
};

# Check for Starlink
die "Must execute a Starlink login" unless (exists $ENV{SURF_DIR});


if ($use_ams) {
  print "Perl/ADAM messaging is present. Good\n";
} else {
  print "Perl/ADAM messaging not available. Using system() instead\n";
}

# Decide whether we are using the new KAPPA (>0.13) or the old
# This governs whether we can use the WCSCOPY command
my $newkappa;
$newkappa = ( -e "$ENV{KAPPA_DIR}/style.def" ? 1 : 0);

# For KAPPA 0.14 this changed to kappa_style.def
unless ($newkappa) {
  $newkappa = ( -e "$ENV{KAPPA_DIR}/kappa_style.def" ? 1 : 0);
}

# Setup default output filename

if (defined $outfile) {
  $outfile =~ s/\.sdf$//;  # Strip .sdf
  $output = $outfile;
} else {
  $output = "final";
}


# Deal with ADAM interface

if ($use_ams) {

  print "Starting monoliths...";
  $Mon{surf_mon} = new ORAC::Msg::ADAM::Task("surf_mon_$$", $ENV{SURF_DIR}."/surf_mon");
  $Mon{kappa_mon} = new ORAC::Msg::ADAM::Task("kappa_mon_$$",$ENV{KAPPA_DIR}."/kappa_mon");
  $Mon{ndfpack_mon} = new ORAC::Msg::ADAM::Task("ndfpack_mon_$$",$ENV{KAPPA_DIR}."/ndfpack_mon");

  # Start ADAM messaging
  $adam = new ORAC::Msg::ADAM::Control;
  $status = $adam->init;
  check_status($status);
  $adam->timeout(1200);     # task timeout of 20 minutes


} else {

  $Mon{surf_mon} = new ORAC::Msg::ADAM::Shell("surf_mon_$$", $ENV{SURF_DIR}."/surf_mon");
  $Mon{kappa_mon} = new ORAC::Msg::ADAM::Shell("kappa_mon_$$",$ENV{KAPPA_DIR}."/kappa_mon");
  $Mon{ndfpack_mon} = new ORAC::Msg::ADAM::Shell("ndfpack_mon_$$",$ENV{KAPPA_DIR}."/ndfpack_mon");

}




$Mon{kappa_mon}->contactw || die "Couldnt contact monoliths";
print "Done\n" if $use_ams;

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

# Generate the weight

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

  my $thrpa_file_suffix = $chop_pa . '_' . $chop_thr;
  $thrpa_file_suffix =~ s/\./p/; # protect against "." in number
  $outwt = 'wt_' . $thrpa_file_suffix;
  $outft = 'ft_' . $thrp_file_suffix;


  # Make it LIKE the current image so that the origin information
  # is correct
  $string = "like=".$frame->file." ftchop=$outft wtchop=$outwt accept";

  $status = $Mon{surf_mon}->obeyw("scumakewt","$string");
  check_status($status);

  # Calculate the total weight (so that we can remove the files
  # when they are no longer needed

  # if this is first time round then simply rename weight to weight
  # else add to weight and rename

  if ($i > 1) {
    # Add to previous
    $status = $Mon{kappa_mon}->obeyw("add","in1=$outwt in2=$wt out=junk$$ title='Total_Weight' reset ");
    rename ("junk$$"."$ext", "$wt$ext");
    # Remove the weight
    unlink($outwt . $ext) or die "Eek $outwt$ext $!";

  } else {
    rename ("$outwt$ext", "$wt$ext");
  }

  # Fourier transform the input data multiplying the DEC (chop_pa=0) by -1
  # This probably implies that I have got the weights inverted for
  # this chop position angle.

  if ($chop_pa == 0) {

    $status = $Mon{kappa_mon}->obeyw("cmult","in=".$frame->file." scalar=-1.0 out=junk$$ reset");
    check_status($status);
    $string = "in=junk$$";
  } else {
    $string = "in=".$frame->file;
  }

  $realout = "re_" . $thrpa_file_suffix;
  $imout = "im_" . $thrpa_file_suffix;

  $status = $Mon{kappa_mon}->obeyw("fourier","$string realout=$realout imagout=$imout hermout=! reset");
  check_status($status);

  # Remove junk.sdf
  unlink "junk$$"."$ext" if (-e "junk$$"."$ext");

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

  $status = $Mon{kappa_mon}->obeyw("mult","in1=$realout in2=$outft out=ijunk$$ reset");
  check_status($status);
  $status = $Mon{kappa_mon}->obeyw("cmult","in=ijunk$$ scalar=-1.0 out=imtemp reset");
  check_status($status);
  $status = $Mon{kappa_mon}->obeyw("mult","in1=$imout in2=$outft out=retemp reset");
  check_status($status);

  # Remove the intermediate files (FFTs)
  unlink "$outft$ext", "$realout$ext", "$imout$ext", "ijunk$$"."$ext";

  # If we have been round before coadd to the running total
  if ($i > 1) {

    $status = $Mon{kappa_mon}->obeyw("add","in1=imtemp in2=$im out=junk$$ reset");

    check_status($status);
    rename ("junk$$"."$ext", "$im$ext");

    $status = $Mon{kappa_mon}->obeyw("add","in1=retemp in2=$re out=junk$$ reset");
    check_status($status);
    rename ("junk$$"."$ext", "$re$ext");
    unlink "imtemp$ext", "retemp$ext";

  } else {
    # else rename files
    rename ("retemp$ext", "$re$ext");
    rename ("imtemp$ext", "$im$ext");
  }


}


# Now we have the total weight, and the processed real and imaginary
# components.

# Remove all the zeroes from the weights array and replace with a small
# number. This is a kludge to prevent DIV from doing weird things
# and crashing on the alpha

$status = $Mon{kappa_mon}->obeyw("substitute","in=$wt out=s$wt oldval=0.0 newval=0.00001");

unlink "$wt$ext";

# Divide real and imaginary by the total weight

$status = $Mon{kappa_mon}->obeyw("div","in1=$re in2=s$wt out=rediv reset");
check_status($status);
$status = $Mon{kappa_mon}->obeyw("div","in1=$im in2=s$wt out=imdiv reset");
check_status($status);

# Remove the weight
unlink "s$wt$ext", "$re$ext", "$im$ext";


# Put filtering in here - have to mask the real and imaginary
# components using ARDMASK, centred on pixel origin (0,0)
# Radius of mask is related to the pixel size and wavelength

if ($filter) {
  my $wlength = $Grp->frame(0)->hdr('WAVELEN');
  my $pixsz   = $Grp->frame(0)->hdr('SCUPIXSZ');

  # Find dimensions of map
  ndf_begin;
  $status = &NDF::SAI__OK;
  ndf_find(&NDF::DAT__ROOT, 'rediv', $indf, $status);
  my @dim = ();
  ndf_dim($indf, 2, @dim, $ndim, $status);
  my (@lbnd, @ubnd);
  ndf_bound($indf, 2, @lbnd, @ubnd, $ndim, $status);
  ndf_annul($indf, $status);
  ndf_end($status);

  # Find radius in pixels
  my $scale = DIAMETER * $pixsz / ($wlength * 206265);

  my $xrad = int (($dim[0] * $scale) + 1);
  my $yrad = int (($dim[1] * $scale) + 1);

  print "Filtering out high frequencies...\n";

  # Mask should be centred on the middle pixel not the pixel 0,0
  my $xcen = int( ( $ubnd[0] + $lbnd[0] )/ 2 );
  my $ycen = int( ( $ubnd[1] + $lbnd[1] )/ 2 );
  print "Pixel centre: $xcen, $ycen  Radius: $xrad, $yrad\n";

  # Create ARD file
  my $ardfile = "ard$$.mask";
  open ARD, "> $ardfile" || die "Error creating ARD file for fourier filter\n";

  if (starversion_gt('kappa', 'V0.18-0')) {
    # ARD will support WCS
    print ARD "COFRAME(PIXEL)\n";
  }
  print ARD ".NOT.ELLIPSE($xcen,$ycen,$xrad,$yrad,0)\n";
  close ARD || die "Error closing ARD file\n";

  my $args = "ardfile=$ardfile";
  $args .= " cosys=world " unless starversion_gt('kappa','V0.18-0');
  $status = $Mon{kappa_mon}->obeyw("ardmask","in=rediv out=rediv_ard $args");
  check_status($status);
  $status = $Mon{kappa_mon}->obeyw("ardmask","in=imdiv out=imdiv_ard $args");
  check_status($status);

  # Rename the files to overwrite the originals
  rename "rediv_ard.sdf", "rediv.sdf";
  rename "imdiv_ard.sdf", "imdiv.sdf";

  unlink $ardfile;
}



print "Running inverse FFT...\n";
# Inverse fourier
$status = $Mon{kappa_mon}->obeyw("fourier","inverse realin=rediv imagin=imdiv out=invfft$output reset");
check_status($status);

# Remove the final junk files
unlink "rediv$ext", "imdiv$ext";

# Now copy the astrometry from one of the input images to the
# output of the FFT. We need to do this since the inverse FFT image
# has lost axis information and WCS (but has retained
# the extensions)

# Since the WCSCOPY command (currently) only copies WCS information and not
# AXIS and origin information we can not use this. We do a kluge instead
# where we use the MATHS command and multiply the input image by 0
# and the output by 1. MATHS propogates WCS and AXIS correctly since
# the input images are identical before and after the FFT

# Select first map as reference
$file = $Grp->frame(0)->file;

$status = $Mon{kappa_mon}->obeyw("maths","exp='IA*0+IB' ia=$file ib=invfft$output out=$output title='SURF:remdbm' novariance reset");
check_status($status);

# Now remove the intermediate file
unlink "invfft$output$ext";

# Finally remove some of the extra FITS keywords relating to chops that
# should not be in the header of the deconvolved image
foreach my $key ('CHOP_PA', 'CHOP_THR', 'CHOP_CRD') {
  $status = $Mon{ndfpack_mon}->obeyw("fitsmod","ndf=$output keyword=$key edit=delete");
  last if $status != 0;
}

print "Result stored in $output\n";


exit;

# Status check

sub check_status {
  my $status = shift;

  croak "Bad status: $status" if $status != 0;


}


################## INCLUDE ORAC CLASSES HERE ####################

package ORAC::Group::JCMT;

# A package to describe the GROUP entity for the pipeline

use 5.004;
use Carp;
use strict;
use NDF;

# Setup the object structure

# NEW - create new instance of Frame

sub new {

  my $proto = shift;
  my $class = ref($proto) || $proto;

  my $group = {};  # Anon hash

  $group->{Name} = undef;
  $group->{Members} = [];
  $group->{Header} = undef;
  $group->{File} = undef;
  $group->{Recipe} = undef;

  bless($group, $class);

  # If an arguments are supplied then we can configure the object
  # Currently the argument will simply be the group name (ID)

  if (@_) {
    $group->name(shift);
  }

  return $group;

}


# Create some methods to access "instance" data
#
# With args they set the values
# Without args they only retrieve values


# Return/set the current file name of the object
# Make sure that the extension is not present



sub name {
  my $self = shift;
  if (@_) { $self->{Name} = shift;}
  return $self->{Name};
}




sub file {
  my $self = shift;
  if (@_) { $self->{File} = $self->stripfname(shift); }
  return $self->{File};
}



# Method to populate the header with a hash
# Requires a hash reference and returns a hash reference



sub header {
  my $self = shift;

  if (@_) {
    my $arg = shift;
    croak("Argument is not a hash reference") unless ref($arg) eq "HASH";
    $self->{Header} = $arg;
  }


  return $self->{Header};
}

# Method to return the recipe name
# If an argument is supplied the recipe is set to that value
# The recipe name can not be set automatically since it relies
# on the members of the group.

#=item recipe
#
#Set or retrieve the name of the recipe being used to reduce the
#group.
#
#    $Grp->recipe("recipe_name");
#    $recipe_name = $Grp->recipe;
#
#=cut


#sub recipe {
#  my $self = shift;
#  if (@_) { $self->{Recipe} = shift;}
#  return $self->{Recipe};
#}


# Method to set/return all members of the group
# This takes an array as input
# An array is returned


sub members {
  my $self = shift;
  if (@_) { @{ $self->{Members} } = @_;}
  return @{ $self->{Members} };
}

# This method returns the reference to the array


sub aref {
  my $self = shift;

  if (@_) {
    my $arg = shift;
    croak("Argument is not an array reference") unless ref($arg) eq "ARRAY";
    $self->{Members} = $arg;
  }

  return $self->{Members};
}


# General methods


# Supply a method to access individual pieces of header information
# Without forcing the user to access the hash directly



sub hdr {
  my $self = shift;

  my $keyword = shift;

  if (@_) { ${$self->header}{$keyword} = shift; }

  return ${$self->header}{$keyword};
}



# Methods for dealing with the members

# Method to push data onto the group
# Multiple members can be added in one go


sub push {
  my $self = shift;
  if (@_) {
    push(@{$self->{Members}}, @_);
  }
}

# Method to access a member by number

# Must supply a number
# Optionally can also supply a value that the nth frame should take
# Presumed to contain objects derived from ORAC::Frame



sub frame {

  my $self = shift;

  my $number = shift;

  # Seems that we are setting the value
  if (@_) { ${$self->aref}[$number] = shift; }

  # Return the value
  return ${$self->aref}[$number];
}


# Method to return the number of frames in a group
# Same style as for $#array.


sub num {

  my $self = shift;

  return $#{$self->aref};

}



sub membernumbers {

  my $self = shift;

  my @list = ();
  foreach my $member ($self->members) {

    push(@list, $member->number);

  }
  return @list;
}


sub membernames {

  my $self = shift;

  # If arguments are supplied use the values to update the
  # filenames in each frame
  if (@_) {
    # Only attempt this if the number of arguments supplied matches
    # The number of members in the group
    if ($self->num == $#_) {
      foreach my $member ($self->members) {
	my $newname = shift;
	$member->file($newname);
      }
    }

  }

  # Now return the list of names associated with each member
  my @list = ();
  foreach my $member ($self->members) {

    push(@list, $member->file);

  }
  return @list;
}


sub inout {

  my $self = shift;

  # Find the suffix
  my $suffix = shift;

  # Initialise the output arrays
  my @in = ();
  my @out = ();

  # Now loop over the members
  foreach my $member ($self->members) {

    # Retrieve the input and output names of these files
    my ($in, $out) = $member->inout($suffix);
    push(@in, $in);
    push(@out, $out);

  }

  # Return the array references
  return \@in, \@out;

}


sub updateout {
  my $self = shift;

  my $suffix = shift;

  # Now loop over the members
  foreach my $member ($self->members) {

    my ($in, $out) = $member->inout($suffix);
    $member->file($out);
  }

  return 1;
}


sub template {
  my $self = shift;
  my $template = shift;

  # Loop over the members
  foreach my $member ($self->members) {
    $member->template($template);
  }
}


sub lastmember {
  my $self = shift;
  my $member = shift;

  if ($member eq $self->frame($self->num)) {
    return 1;
  }

  return 0;
}



sub reduce {
  my $self = shift;

  return $self->frame($self->num);

}



# Private method for removing file extensions from the filename strings
# In the base class this does nothing. It is up to the derived classes
# To do something special with this.



sub readhdr {

  my $self = shift;

  # Just read the NDF fits header
  my ($ref, $status) = fits_read_header($self->file);

  # Return an empty hash if bad status
  $ref = {} if ($status != &NDF::SAI__OK);

  # Set the header in the group
  $self->header($ref);

  return $ref;

}




sub membernamessub {

  my $self = shift;
  my $sub = lc(shift);

  my @list = ();

  # Loop through each frame
  foreach my $frm ($self->members) {

    # Loop through each sub instrument
    my @subs = $frm->subs;
    for (my $i=0; $i < $frm->nsubs; $i++) {
      push (@list, $frm->file($i+1)) if $sub eq lc($subs[$i]);
    }
  }

  return @list;

}


sub grpoutsub {
  my $self = shift;

  # dont bother checking whether something was specified
  my $sub = shift;

  # Retrieve the root name
  my $file = $self->file;

  # Set suffix
  my $suffix = '_' . lc($sub);

  # Append the sub-instrument (don't if the sub is already there!
  $file .= $suffix unless $file =~ /$suffix$/;

  return $file;
}

sub stripfname {

  my $self = shift;

  my $name = shift;

  # Strip everything after the first dot
  $name =~ s/\.(sdf)(\.gz|\.Z)?$//;

  return $name;

}




package ORAC::Frame::JCMT;


use strict;
use Carp;

use NDF; # For fits reading

sub new {

  my $proto = shift;
  my $class = ref($proto) || $proto;

  my $frame = {};  # Anon hash

  $frame->{RawName} = undef;
  $frame->{Header} = undef;
  $frame->{Group} = undef;
  $frame->{Files} = [];
  $frame->{Recipe} = undef;
  $frame->{Nsubs} = undef;
  $frame->{Subs} = [];
  $frame->{Filters} = [];
  $frame->{WaveLengths} = [];

  bless($frame, $class);

  # If arguments are supplied then we can configure the object
  # Currently the argument will be the filename.
  # This could be extended to include a reference to a hash holding the
  # header info but this may well compromise the object since
  # the best way to generate the header (including extensions) is to use the
  # readhdr method.

  if (@_) {
    $frame->configure(@_);
  }

  return $frame;

}

sub file {

  # May be able to drop all the wacky returns if we let


  my $self = shift;

  # Set it to point to first member by default
  my $index = 0;

  # Check the arguments
  if (@_) {

    my $firstarg = shift;

    # If this is an integer then proceed
    # Check for int and non-zero (since strings eval as 0)
    # Cant use int() since this extracts integers from the start of a
    # string! Match a string containing only digits
    if ($firstarg =~ /^\d+$/ && $firstarg != 0) {

      # Decrement value so that we can use it as a perl array index
      $index = $firstarg - 1;

      # If we have more arguments we are setting a value
      # else wait until we return the specified value
      if (@_) {
         ${$self->aref}[$index] = $self->stripfname(shift);
      }
    } else {
      # Just set the first value
      ${$self->aref}[$index] = $self->stripfname($firstarg);
    }
  }

  # If index is greater than number of files stored in the
  # array return the first one
  $index = 0 if ($index > $self->num);

  # Nothing else of interest so return specified member
  return ${$self->aref}[$index];

}

=item configure

This method is used to configure the object. It is invoked
automatically if the new() method is invoked with an argument. The
file(), raw(), readhdr(), header(), group() and recipe() methods are
invoked by this command. Arguments are not required.

The sub-instrument configuration is also stored.

=cut

sub configure {
  my $self = shift;

  my $fname = shift;

  # Set the filename
  $self->file($fname);

  # Set the raw data file name
  $self->raw($fname);

  # Populate the header
  $self->header($self->readhdr);

  # Find the group name and set it
#  $self->group($self->findgroup);

  # Find the recipe name
#  $self->recipe($self->findrecipe);

  # Find number of sub-instruments from header
  # and store this value along with all sub-instrument info.
  # Do this so that the header can be changed without us
  # losing the original state information
#  $self->nsubs($self->findnsubs);
#  $self->subs($self->findsubs);
#  $self->filters($self->findfilters);
#  $self->wavelengths($self->findwavelengths);

  # Return something
  return 1;
}



=item readhdr

Reads the header from the observation file (the filename is stored
in the object). The reference to the header hash is returned.
This method does not set the header in the object (in general that
is done by configure() ).

    $hashref = $Obj->readhdr;

If there is an error during the read a reference to an empty hash is
returned.

Currently this method assumes that the reduced group is stored in
NDF format. Only the FITS header is retrieved from the NDF.

There are no input arguments.

=cut

sub readhdr {

  my $self = shift;

  # Just read the NDF fits header
  my ($ref, $status) = fits_read_header($self->file);

  # Return an empty hash if bad status
  $ref = {} if ($status != &NDF::SAI__OK);

  return $ref;
}


=item findgroup

Return the group associated with the Frame.
This group is constructed from header information.

For remdbm - they should all be in the group so return a constant.

=cut

# Supply a new method for finding a group


sub findgroup {

  my $self = shift;

  # construct group name
  my $group = 'REMDBM';

  return $group;

}


=item inout

Method to return the current input filename and the
new output filename given a suffix and a sub-instrument
number.
Currently the suffix is simply appended to the input.

Returns $in and $out in an array context:

  ($in, $out) = $Obs->inout($suffix, $num);

The second argument indicates the sub-instrument number
and is optional (defaults to first sub-instrument).
If only one file is present then that is used as $infile.
(handled by the file() method.)

=cut

sub inout {
  my $num;

  my $self = shift;

  my $suffix = shift;

  # Find the sub-instrument
  if (@_) {
    $num = shift;
  } else {
    $num = 1;
  }

  my $infile = $self->file($num);
  my $outfile = $infile . $suffix;

  return ($infile, $outfile);

}



=back

=head1 NEW METHODS FOR JCMT

This section describes methods that are available in addition
to the standard methods found in ORAC::Frame.

=over 4

=item files

Set or retrieve the array containing the current file names
associated with the frame object.

    $Obj->members(@files);
    @files = $Obj->files;

=cut

sub files {
  my $self = shift;
  if (@_) { @{ $self->{Files} } = @_;}
  return @{ $self->{Files} };
}


=item aref

Set or retrieve the reference to the array containing the members of the
group.

    $Obj->aref(\@files);
    $arrayref = $Obj->aref;

=cut


sub aref {
  my $self = shift;

  if (@_) {
    my $arg = shift;
    croak("Argument is not an array reference") unless ref($arg) eq "ARRAY";
    $self->{Files} = $arg;
  }

  return $self->{Files};
}




=item num

Return the number of files in a group minus one.
This is identical to the $# construct.

  $number_of_files = $Obj->num;

=cut

sub num {

  my $self = shift;

  return $#{$self->aref};

}

=item nsubs

Return the number of sub-instruments associated with the Frame.

=cut

sub nsubs {
  my $self = shift;

  if (@_) { $self->{Nsubs} = shift; };

  return $self->{Nsubs};

}


=item subs

Return or set the names of the sub-instruments associated
with the frame.

=cut

sub subs {
  my $self = shift;

  if (@_) {
    @{$self->{Subs}} = @_;
  }

  return @{$self->{Subs}};

}

=item filters

Return or set the filter names associated with each sub-instrument
in the frame.

=cut

sub filters {
  my $self = shift;

  if (@_) {
    @{$self->{Filters}} = @_;
  }

  return @{$self->{Filters}};

}

=item wavelengths

Return or set the wavelengths associated with each  sub-instrument
in the frame.

=cut

sub wavelengths {
  my $self = shift;

  if (@_) {
    @{$self->{WaveLengths}} = @_;
  }

  return @{$self->{WaveLengths}};

}

=item findnsubs

Forces the object to determine the number of sub-instruments
associated with the data by looking in the header().
The result can be stored in the object using nsubs().

Unlike findgroup() this method will always search the header for
the current state.

=cut

sub findnsubs {
  my $self = shift;

  return $self->hdr('N_SUBS');
}



=item findsubs

Forces the object to determine the names of all sub-instruments
associated with the data by looking in the header().
The result can be stored in the object using subs().

Unlike findgroup() this method will always search the header for
the current state.

=cut

sub findsubs {
  my $self = shift;

  # Dont use the nsubs method (derive all from header)
  my $nsubs = $self->hdr('N_SUBS');

  my @subs = ();
  for (my $i =1; $i <= $nsubs; $i++) {
    my $key = 'SUB_' . $i;

    push(@subs, $self->hdr($key));
  }

  # Should now set the value in the object!

  return @subs;
}



=item findfilters

Forces the object to determine the names of all sub-instruments
associated with the data by looking in the header().
The result can be stored in the object using subs().

Unlike findgroup() this method will always search the header for
the current state.

=cut


sub findfilters {
  my $self = shift;

  # Dont use the nsubs method (derive all from header)
  my $nsubs = $self->hdr('N_SUBS');

  my @filter = ();
  for (my $i =1; $i <= $nsubs; $i++) {
    my $key = 'FILT_' . $i;

    push(@filter, $self->hdr($key));
  }

  return @filter;
}


=item findwavelengths

Forces the object to determine the names of all sub-instruments
associated with the data by looking in the header().
The result can be stored in the object using subs().

Unlike findgroup() this method will always search the header for
the current state.

=cut


sub findwavelengths {
  my $self = shift;

  # Dont use the nsubs method (derive all from header)
  my $nsubs = $self->hdr('N_SUBS');

  my @filter = ();
  for (my $i =1; $i <= $nsubs; $i++) {
    my $key = 'WAVE_' . $i;

    push(@filter, $self->hdr($key));
  }

  return @filter;
}



=back

=head1 PRIVATE METHODS

The following methods are intended for use inside the module.
They are included here so that authors of derived classes are
aware of them.

=over 4

=item stripfname

Method to strip file extensions from the filename string. This method
is called by the file() method. For UKIRT we strip all extensions of the
form ".sdf", ".sdf.gz" and ".sdf.Z" since Starlink tasks do not require
the extension when accessing the file name.

=cut

sub stripfname {

  my $self = shift;

  my $name = shift;

  # Strip everything after the first dot
  $name =~ s/\.(sdf)(\.gz|\.Z)?$//;

  return $name;
}


# Setup the object structure



# Create some methods to access "instance" data
#
# With args they set the values
# Without args they only retrieve values


# Method to return/set the filename of the raw data
# Initially this is the same as {File}


=item raw

This method returns (or sets) the name of the raw data file
associated with this object.

  $Obs->raw("raw_data");
  $filename = $Obs->raw;

=cut

sub raw {
  my $self = shift;
  if (@_) { $self->{RawName} = shift; }
  return $self->{RawName};
}


# Return/set the current file name of the object
# Make sure that the extension is not present



# Method to return group
# If an argument is supplied the group is set to that value
# If the group is undef then the findgroup method is invoked to set it


=item group

This method returns the group name associated with the observation.
If the object has a value of undef (ie a new object) the findgroup()
method is automatically invoked to determine the group. Subsequent
invocations of the group method will simply return the current value.
The group name can be set explicitly but in general the automatic
lookup should be used.

  $group_name = $Obs->group;
  $Obs->group("group");

=cut


sub group {
  my $self = shift;
  if (@_) { $self->{Group} = shift;}

  unless (defined $self->{Group}) {
    $self->findgroup;
  }

  return $self->{Group};
}

# Method to return the recipe name
# If an argument is supplied the recipe is set to that value
# If the recipe is undef then the findrecipe method is invoked to set it


=item recipe

This method returns the recipe name associated with the observation.
If the object has a value of undef (ie a new object) the findrecipe()
method is automatically invoked to determine the recipe. Subsequent
invocations of the method will simply return the current value.
The recipe name can also be set explicitly but in general this behaviour
would be superceded by ORAC::Group objects.

  $recipe_name = $Obs->recipe;
  $Obs->recipe("recipe");

=cut


sub recipe {
  my $self = shift;
  if (@_) { $self->{Recipe} = shift;}

  unless (defined $self->{Recipe}) {
    $self->findrecipe;
  }

  return $self->{Recipe};
}

# Method to populate the header with a hash
# Requires a hash reference and returns a hash reference

=item header

Set or retrieve the hash associated with the header information
stored for the observation.

    $Obs->header(\%hdr);
    $hashref = $Obs->header;

This methods takes and returns a reference to a hash.

=cut


sub header {
  my $self = shift;

  if (@_) {
    my $arg = shift;
    croak("Argument is not a hash") unless ref($arg) eq "HASH";
    $self->{Header} = $arg;
  }


  return $self->{Header};
}


# Supply a method to access individual pieces of header information
# Without forcing the user to access the hash directly

=item hdr

This method allows specific entries in the header to be accessed.
The header must be available (set by the "header" method).
The input argument should correspond to the keyword in the header
hash.

  $tel = $Obs->hdr("TELESCOP");
  $instrument = $Obs->hdr("INSTRUME");

Can also be used to set values in the header.

  $Obs->hdr("INSTRUME", "IRCAM");

=cut

sub hdr {
  my $self = shift;

  my $keyword = shift;

  if (@_) { ${$self->header}{$keyword} = shift; }

  return ${$self->header}{$keyword};
}







# Supply a method to find the recipe name and set it

=item findgroup

Method to determine the recipe name that should be used to reduce
the observation.
The default method is to look for a "RECIPE" entry in the header.

  $recipe = $Obs->findrecipe;

=cut


sub findrecipe {
  my $self = shift;

  # Simplistic routine that simply returns the RECIPE
  # entry in the header

  return $self->hdr('RECIPE');

}


# Supply a method to return the number associated with the observation

=item number

Method to return the number of the observation. The number is
determined by looking for a number at the end of the raw data
filename.  For example a number can be extracted from strings of the
form textNNNN.sdf or textNNNN, where NNNN is a number (leading zeroes
are stripped) but not textNNNNtext (number must be followed by a decimal
point or nothing at all).

  $number = $Obs->number;

=cut


sub number {

  my $self = shift;

  my ($number);

  # Get the number from the raw data
  # Assume there is a number at the end of the string
  # (since the extension has already been removed)
  # Leading zeroes are dropped

  if ($self->raw =~ /(\d+)(\.\w+)?$/) {
    # Drop leading 00
    $number = $1 * 1;
  } else {
    # No match so set to -1
    $number = -1;
  }

  return $number;

}



=item template

Method to change the current filename of the frame (file())
so that it matches the current template. e.g.:

  $Obs->template("something_number_flat")

Would change the current file to match "something_number_flat".
Essentially this simply means that the number in the template
is changed to the number of the current frame object.

The base method assumes that the filename matches the form:
prefix_number_suffix. This must be modified by the derived
classes since in general the filenaming convention is telescope
and instrument specific.

=cut

sub template {
  my $self = shift;
  my $template = shift;

  my $num = $self->number;
  # Change the first number
  $template =~ s/_\d+_/_${num}_/;

  # Update the filename
  $self->file($template);

}


=item userheader

Set or retrieve a hash containing general purpose information
about the frame. This is distinct from the Frame header
(see header()) that is associated with the FITS header.

    $Obs->userheader(\%hdr);
    $hashref = $Obs->userheader;

This methods takes and returns a reference to a hash.

=cut


sub userheader {
  my $self = shift;

  if (@_) {
    my $arg = shift;
    croak("Argument is not a hash") unless ref($arg) eq "HASH";
    $self->{UserHeader} = $arg;
  }


  return $self->{UserHeader};
}


=item uhdr

This method allows specific entries in the user specified header to
be accessed. The header must be available (set by the "userheader" method).
The input argument should correspond to the keyword in the header
hash.

  $info = $Obs->uhdr("INFORMATION");

Can also be used to set values in the header.

  $Obs->uhdr("INFORMATION", "value");

=cut

sub uhdr {
  my $self = shift;

  my $keyword = shift;

  if (@_) { ${$self->userheader}{$keyword} = shift; }

  return ${$self->userheader}{$keyword};
}

package ORAC::Msg::ADAM::Shell;

use strict 'vars';
no strict 'subs';
use Carp;

# Need this to perform the get() function
use NDF;

# Need to strip a path
use File::Basename;

# Safe current directory
use Cwd qw/getcwd/;

# Retrieve the status constants
use constant ORAC__OK => 0;
use constant ORAC__ERROR => -1;


sub new {
  my $proto = shift;
  my $class = ref($proto) || $proto;

  my $task = {};  # Anon hash

  # Need to store some information
  $task->{Name} = undef;
  $task->{Monolith} = undef;
  $task->{Path}  = undef;

  # Set to current directory when initialised
  $task->{Cwd} = getcwd;

  # Bless task into class
  bless($task, $class);

  # If we have arguments then we are trying to do a load
  # as well
  if (@_) { $task->load(@_); };

  return $task;
}


# Provide methods for accessing and setting instance data
# Most are private except for cwd() which is a published method

# Store and access the messaging name
# Not overly useful but keep it any way

sub name {
  my $self = shift;
  if (@_) { $self->{Name} = shift; }
  return $self->{Name};
}

# Name of the monolith that we are trying to run
# Note that we do not run this monolith directly since we run through
# a link in the file system

sub mon {
  my $self = shift;
  if (@_) { $self->{Monolith} = shift; }
  return $self->{Monolith};
}


# Location of monolith in the file system

sub path {
  my $self = shift;
  if (@_) { $self->{Path} = shift; }
  return $self->{Path};
}

=item cwd

Set and retrieve the directory in which this monolith
should operate.

  ($cwd, $status) = $obj->cwd("newdir")
  ($cwd, $status) = $obj->cwd;

If the specified directory does not exist, bad status is
returned and the cwd is not changed.

=cut

sub cwd {
  my $self = shift;
  my $status = ORAC__OK;

  # Supply an argument
  if (@_) {
    my $cwd = shift;

    # Check that the directory exists
    if (-d $cwd) {
      $self->{Cwd} = $cwd;
    } else {
      $status = ORAC__ERROR;
    }

  }

  # Need to return a status since this is part of the
  # standard interface
  return ($self->{Cwd}, $status);

}


=item load

Initialise the monolith into the object. What this really
does is store the directory of the monolith
so that it can be run and so that we can determine
which tasks are linked to it.

In reality the second argument is mandatory for this
interface since I have no idea where the monolith
is otherwise.

=cut

sub load {
  my $self = shift;

  my $name = shift;
  $self->name($name);

  # A further argument (optional) will be the monolith name
  if (@_) {
    my $monolith = shift;

    # Need to separate monolith name from the path
    my ($mon, $path, $junk) = fileparse($monolith);

    # Check for a path. If current directory
    # then expand since I have no other idea
    if ($path eq "./") {
      $path = getcwd;
    }

    $self->path($path);
    $self->mon($mon);

  }

  # Have to return a status
  return ORAC__OK;
}


=item obeyw

Execute an ADAM task via the unix shell.
Return the shell exit status.

  $status = obeyw("task", "arguments");

Full path to "task" is not required since this was setup
when the object was initialised via load().

Note that currently we have no control over the output
messages. It is conceivable that I could at least
redirect to /dev/null if a flag was set in the
ControlSH module.

=cut

sub obeyw {
  my $self = shift;

  my $task = shift;
  my $args = shift || " ";

  my $command = $self->path . "/" . $task;

  # Check that we can actually execute the command
  return ORAC__ERROR unless (-x $command);

  # Change to the current working directory before running
  # This probably has some overhead
  my $cwd = getcwd;

  my ($mondir, $junk) = $self->cwd;
  chdir($mondir) || croak "Error changing directory to $mondir";

  # The Args must be modified so that quotes are escaped
  # before they go to the shell
  # Same problem with commas and brackets.

  $args =~ s/\[/\\\[/g;
  $args =~ s/\]/\\\]/g;
  $args =~ s/\)/\\\)/g;
  $args =~ s/\(/\\\(/g;
  $args =~ s/~/\\~/g;

  # Now try to replace single quotes with a "' '" combination
  # and only do this for strings containing commas
  # Probably will not work if we have a '{xx,xx}' combination

  $args =~ s/\'(\w+,\w+)+\'/\"$&\"/g;

  my $exstat = system("$command $args");

  # Now change directory back again
  chdir($cwd) || croak "Error changing back to current directory\n";

  return ORAC__OK if $exstat == 0;
  return $exstat;

}


=item get

Retrieve the current value of a parameter

  ($status, @values) = $obj->get("task", "param");

The first argument returned is the ORAC status. All
subsequent arguments are the parameter values (in an array
context)

=cut


sub get {
  my $self = shift;

  my $task = shift;
  my $param = shift;
  my $status = &NDF::SAI__OK;

  my (@values) = par_get($param, $task, \$status);

  $status = ORAC__OK if ($status == &NDF::SAI__OK);

  return ($status, @values);
}


=item set

Set a parameter.
Currently not implemented

=cut

sub set {


  return ORAC__OK;

}


=item control

Control current working directory and parameter resets.  The type of
control message is specified via the first argument. Allowed values
are:

  default:  Return or set the current working directory
  par_reset: Reset all parameters associated with the monolith.

  ($current, $status) = )$obj->control("type", "value")

"value" is only relevant for the "default" type and is used
to specify a new working directory. $current is always returned
even if it is undefined.

These commands are synonymous with the cwd() and resetpars()
methods.

=cut

sub control {
  my $self = shift;
  my ($status);

  my $type = shift;

  if ($type eq "default") {
    my $newdir = shift;

    # An argument was passed
    if (defined $newdir) {
      $self->cwd($newdir);
    } else {
      # no argument
      ($newdir, $status) = $self->cwd
    }
    return $newdir, $status;
  } elsif ($type eq "par_reset") {

    $status = $self->resetpars;
    return (undef, $status);

  } else {
    croak "Unrecognised control type. Should be \'default\' or '\par_reset\'";
  }

}


=item resetpars

Reset parameter values.
A simplistic version is implemented that tries to remove
parameter files from the current ADAM_USER directory
depending on whether a link exists in the monolith directory.

Do nothing for now!

=cut

sub resetpars {
  my $self = shift;


  return ORAC__OK;
}


=item contact and contactw

Check that we can contact the monolith.
This method simply makes sure that we know where the monolith
is and that it can be executed. There is no difference between
contactw. and contact(). Returns a '1' if the command can be executed
and '0' if it cannot.

=cut

sub contact {
  my $self = shift;

  my $command = $self->path . "/" . $self->mon;

  # Check that we can actually execute the command
  return 0 unless (-x $command);

  return 1;
}


sub contactw {
  my $self = shift;

  return $self->contact;

}


=back

=head1 AUTHOR

Tim Jenness (t.jenness@jach.hawaii.edu).
and Frossie Economou (frossie@jach.hawaii.edu)

=head1 REQUIREMENTS

Requires the C<NDF>, C<Cwd> and C<File::Basename> modules.

=head1 See Also

L<perl>,
L<ORAC::Msg::ADAM::Task>

=cut

package ORAC::Msg::ADAM::Control;

use Carp;
use strict;

# This needs to Starlink module
# use Starlink::AMS::Init;
# use Starlink::ADAM ();

# Derive all methods from the Starlink module since this
# behaves in exactly the same way.

#@ORAC::Msg::ADAM::Control::ISA = qw/Starlink::AMS::Init/;

# Just include the methods that we need to run remdbm.pl

sub new {

  my $proto = shift;
  my $class = ref($proto) || $proto;

  my $task = {};  # Anon hash

  $task->{OBJ} = new Starlink::AMS::Init;

  # Bless this ams into class
  bless($task, $class);

  # Allow for the messaging system to be initiated automatically
  # if a true  argument '1' is passed
  if (@_) {
    my $value = shift;
    if ($value) {
      my $status = $task->obj->init;
      my $errhand = $task->obj->stderr;
      print $errhand "Error initialising messaging system\n"
        unless $status == &Starlink::ADAM::SAI__OK;
    }
  }

  # Return to caller
  return $task;
};


sub obj {
  my $self = shift;
  return $self->{OBJ};
}


sub messages {
  my $self = shift;
  if (@_) {
    my $val = shift;
    $self->obj->messages($val);
  }
  return $self->obj->messages;
}


sub timeout {
  my $self = shift;
  if (@_) {
    my $val = shift;
    $self->obj->timeout($val);
  }
  return $self->obj->timeout;
}

sub init {
  my $self = shift;
  my $status = $self->obj->init;
  return $status;
}




package  ORAC::Msg::ADAM::Task;


use strict;
use Carp;

no strict 'subs';
# Import ORAC constants
use constant ORAC__OK => 0;
use constant ORAC__ERROR => -1;


# I need to import good Starlink status from the ADAM module
#use Starlink::ADAM ();

use vars qw/$DTASK__ACTCOMPLETE $SAI__OK/;

# Access the AMS task code
# use Starlink::AMS::Task;

# Local definition of DTASK__ACT_COMPLETE. Probably should
# try to get it from Starlink::ADAM but currently broken

$DTASK__ACTCOMPLETE = 142115659;

$SAI__OK = &Starlink::ADAM::SAI__OK;

# Cannot subclass methods since I need to change most of them
# anyway.

=item new

Create a new instance of a ORAC::Msg::ADAM::Task object.

  $obj = new ORAC::Msg::ADAM::Task;
  $obj = new ORAC::Msg::ADAM::Task("name_in_message_system","monolith");

If supplied with arguments (matching those expected by load() ) the
specified task will be loaded upon creating the object. If the load()
fails then undef is returned (which will not be an object reference).

=cut


sub new {
  my $proto = shift;
  my $class = ref($proto) || $proto;

  my $task = {};  # Anon hash

  # Since we are really simply handling another object
  # Create the new object (Starlink::AMS::Task) and store it.
  $task->{Obj} = new Starlink::AMS::Task;  # Name in AMS

  # Bless task into class
  bless($task, $class);

  # If we have arguments then we are trying to do a load
  # as well
  if (@_) { $task->load(@_); };

  return $task;
}


# Private method for handling the Starlink::AMS::Task object

sub obj {
  my $self = shift;
  if (@_) { $self->{Obj} = shift; }
  return $self->{Obj};
}


=item load

Load a monolith and set up the name in the messaging system.
This task is called by the 'new' method.

  $status = $obj->load("name","monolith_binary");

If the second argument is omitted it is assumed that the binary
is already running and can be called by "name".

If a path to a binary with name "name" already exists then the monolith
is not loaded.

=cut

sub load {

  my $self = shift;
  # initialise
  my $status = $main::SAI__OK;

  if (@_) { $status = $self->obj->load(@_); }

  # Convert from ADAM to ORAC status
  # Probably should put in a subroutine
  if ($status == $main::SAI__OK) {
    $status = ORAC__OK;
  }

  return $status;
}


=item obeyw

Send an obey to a task and wait for a completion message

  $status = $obj->obeyw("action","params");

=cut

sub obeyw {
  my $self = shift;

  my $status;

  # Pass arguments directly to the object
  if (@_) { $status = $self->obj->obeyw(@_); }

  # Should now change status from the obeyw (DTASK__ACTCOMPLETE)
  # to good ORAC status

  if ($status == $main::DTASK__ACTCOMPLETE) {
    $status = ORAC__OK;
  }
  return $status;

}


=item get

Obtain the value of a parameter

 ($status, @values) = $obj->get("task", "param");

Note that this is a different order to that returned by the
Standard ADAM interface and follows the ORAC definition.

=cut

sub get {
  # Check number of arguments
  if (scalar(@_) != 3) {
    croak 'get: Wrong number of arguments. Usage: $task->get(\'task\', \'param\')';
  }

  my $self = shift;

  # Now need to construct the arguments for the AMS layer

  my $task = shift;
  my $param = shift;

  my $arg = $task .":" . $param;

  my ($result, $status) = $self->obj->get($arg);

  # Convert $result to an array
  my @values = ();

  # an array of values if we have a square bracket at the start
  # something and a square bracket at the end

  if ($result =~ /^\s*\[.*\]\s*$/) {
    # Remove the brackets
    $result =~ s/^\s*\[(.*)]\s*/$1/;

    # Now split on comma
    @values = split(/,/, $result);

  } else {
    push(@values, $result);
  }

  # DOUBLE precision values are not handled by perl as numbers
  # We need to change all parameters values of form "numberD+-number"
  # to "numberE+-number"
  # not clear whether I should be doing this in the ADAM module
  # itself or here. For now do it in the ORAC interface

  map { s/(\d)D(\+|-\d)/${1}E$2/g; } @values;

  # Convert from ADAM to ORAC status
  # Probably should put in a subroutine
  if ($status == $main::SAI__OK) {
    $status = ORAC__OK;
  }

  return ($status, @values);
}

=item set

Set the value of a parameter

  $status = $obj->set("task", "param", "newvalue");

=cut

sub set {
  # Check number of arguments
  if (scalar(@_) != 4) {
    croak 'get: Wrong number of arguments. Usage: $task->set(\'task\', \'param\', \'newvalue\')';
  }

  my $self = shift;

  # Now need to construct the arguments for the AMS layer

  my $task = shift;
  my $param = shift;
  my $value = shift;

  my $arg = $task .":" . $param;

  my $status = $self->obj->set($arg, $value);

  # Convert from ADAM to ORAC status
  # Probably should put in a subroutine
  if ($status == $main::SAI__OK) {
    $status = ORAC__OK;
  }

  return $status;
}


=item control

Send CONTROL messages to the monolith. The type of control
message is specified via the first argument. Allowed values are:

  default:  Return or set the current working directory
  par_reset: Reset all parameters associated with the monolith.

  ($current, $status) = )$obj->control("type", "value")

"value" is only relevant for the "default" type and is used
to specify a new working directory. $current is always returned
even if it is undefined.

These commands are synonymous with the cwd() and resetpars()
methods.

=cut

sub control {

  my ($value, $status);
  my $self = shift;

  if (@_) {
    ($value, $status) = $self->obj->control(@_);

    # Convert from ADAM to ORAC status
    # Probably should put in a subroutine
    if ($status == $main::SAI__OK) {
      $status = ORAC__OK;
    }
  }
  return ($value, $status);
}

# Stop the monolith from being killed on exit

sub forget {
  my $self = shift;
  $self->obj->forget;
}

=item resetpars

Reset all parameters associated with a monolith

  $status = $obj->resetpars;

=cut

sub resetpars {
  my $self = shift;

  my ($junk, $status) = $self->obj->control("par_reset");

  # Convert from ADAM to ORAC status
  # Probably should put in a subroutine
  if ($status == $main::SAI__OK) {
    $status = ORAC__OK;
  }

  return $status;

}


=item cwd

Set and retrieve the current working directory of the monolith

  ($cwd, $status) = $obj->cwd("newdir");

=cut

sub cwd {
  my $self = shift;
  my $newdir = shift;

  my ($value, $status) = $self->obj->control("default", $newdir);

  # Convert from ADAM to ORAC status
  # Probably should put in a subroutine
  if ($status == $main::SAI__OK) {
    $status = ORAC__OK;
  }
  return $status;

}


=item contactw

This method will not return unless the monolith can be contacted.
It only returns with a timeout. Returns a '1' if we contacted okay
and a '0' if we timed out. It will timeout if it takes longer than
specified in ORAC::Msg::ADAM::Control->timeout.

=cut


sub contactw {
  my $self = shift;
  return $self->obj->contactw;
}


=item contact

This method can be used to determine whether the object can
contact a monolith. Returns a 1 if we can contact a monolith and
a zero if we cant.

=cut

sub contact {
  my $self = shift;
  return $self->obj->contact;
}

=back

=head1 REQUIREMENTS

This module requires the Starlink::AMS::Task module.

=head1 SEE ALSO

L<Starlink::AMS::Task>

=head1 AUTHORS

Tim Jenness (t.jenness@jach.hawaii.edu)
and Frossie Economou (frossie@jach.hawaii.edu)

=cut


__END__


*+
*  Name:
*    REMDBM

*  Purpose:
*    Remove dual beam signature from scan maps

*  Language:
*    Perl 5

*  Description:
*    This program should be used to reduce SCAN-MAP data taken
*    using the technique described by Emerson (1995, ASP Conf Ser 75, 309).
*    The deconvolution is performed using Fast Fourier techniques.

*  Usage:
*    remdbm [-h] [-v] [-out=] files

*  ADAM Parameters:
*    -h
*      Help message
*    -v
*      Version number. Also indicates whether ADAM communication is
*      enabled.
*    -out=file
*      Filename of output image. Default is 'final.sdf'
*    -filter
*      Filter out frequencies higher than can be detected by the telescope.
*    files
*      List of input files to be processed. Shell wildcards are allowed.
*      See notes for restrictions.

*  Examples:
*    remdbm *_reb.sdf
*      Process all files matching the pattern.
*    remdbm -out=m82 o66_lon_reb o67_lon_reb o68_lon_reb o69_lon_reb
*      Process the four input images. The output filename is set to
*      m82.sdf.

*  Notes:
*    The following restrictions apply:
*    - Each image should contain a single chop configuration.
*    - Each image must have identical dimensions and pixel size
*      (they do not need to be square)
*    - The images must be rebinned in the same coordinate system
*      as the chop throw.

*  Authors:
*    Tim Jenness (JAC)

*  Copyright:
*     Copyright (C) 1995,1996,1997,1998,1999 Particle Physics and Astronomy
*     Research Council. All Rights Reserved.


*  Related Applications:
*    SURF: SCUMAKEWT
*    KAPPA: FOURIER
