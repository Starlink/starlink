#!/star/Perl/bin/perl -W
#
# Autoastrometry tool
#
# Uses the facilities in StarPerl, so might not work with a vanilla
# Perl installation.  See SUN/228 for details.
#
# Environment variables used:
#
#    STARLINK (default /star):
#        Root of the starlink tree.
#    AUTOASTROM_DIR (default from $0):
#        Location of the autoastrom configuration files
#    AUTOASTROM_TEMPDIR (default /tmp):
#        Location for temporary autoastrom files
#    EXTRACTOR_DIR (default $STARLINK/bin/extractor):
#        Location of the SExtractor binary and config files
#    KAPPA_DIR (default $STARLINK/bin/kappa):
#        Location of the kappa binary and config files
#
# $Id$

use strict;

my $Usage = "$0 [--[no]keep] [--temp=dir] [--timeout=n] [--[no]messages] <NDF>";
my $progpath;			# Path to program.
($progpath = $0) =~ s+/[^/]*$++;

my $NDF = undef;		# Name of the NDF to be processed
                                # (that is, without the .sdf
                                # extension) 

my $keeptemps = 1;		# Keep temporary files for later debugging.

my $tempDir = undef;		# Allow this to be forced on the
                                # commandline.  This is presently
                                # mostly for debugging, but could be
                                # used in some production modes to
                                # redo some processing.

#$noregenerate = 0;		# If true, don't regenerate
                                # preexisting files (probably only
                                # used in conjunction with --temp to
                                # redo some processing).

my $monolithTimeout = 120;	# Timeout for monoliths.  Needs to be
                                # larger than default 60s.

my $monolithMessages = 1;	# Display monolith messages.

my $maxobj = 500;		# Maximum number of objects to
                                # manipulate.  This number is
                                # fairly arbitrary, but generous.

my $esocatname = 'usno@eso';	# Name of the catalogue to use by
                                # default, as known by SkyCat.

my $skycatconfig = undef;	# SkyCat configuration file.

# Global variables
my @TEMPFILES = ( );		# List of the temporary files which
                                # are created.



# Check that all the required environment variables have values.
$ENV{STARLINK} = '/star'
  unless defined ($ENV{STARLINK});
$ENV{AUTOASTROM_DIR} = "$progpath"
  unless defined ($ENV{AUTOASTROM_DIR});
$ENV{AUTOASTROM_TEMPDIR} = "/tmp"
  unless defined $ENV{AUTOASTROM_TEMPDIR};
$ENV{EXTRACTOR_DIR} = "$ENV{STARLINK}/bin/extractor"
  unless defined ($ENV{EXTRACTOR_DIR});
$ENV{KAPPA_DIR} = "$ENV{STARLINK}/bin/kappa"
  unless defined ($ENV{KAPPA_DIR});
$ENV{CCDPACK_DIR} = "$ENV{STARLINK}/bin/ccdpack"
  unless defined $ENV{CCDPACK_DIR};
$ENV{ATOOLS_DIR} = "$ENV{STARLINK}/bin/atools"
  unless defined $ENV{ATOOLS_DIR};
$ENV{ASTROM_DIR} = "$ENV{AUTOASTROM_DIR}"
  unless defined $ENV{ASTROM_DIR};
$ENV{CONVERT_DIR} = "$ENV{STARLINK}/bin/convert"
  unless defined $ENV{CONVERT_DIR};



# Include the ADAM interface modules, and version-checking module
use Starlink::AMS::Init;
use Starlink::AMS::Task;
use Starlink::Versions;
# Include the NDF modules, for the fits_read_header function
use NDF;

# Use the getopt library
use Getopt::Long;


# The following requires that the variable AUTOASTROM_DIR be defined
# at _compile_ time (so that the setting of it above will never
# actually happen).
use lib "$ENV{AUTOASTROM_DIR}";
use Moggy;


# Include the subroutines
use autoastrom;


# Check we have the appropriate versions of applications.  We need
# CCDPACK version 4.0.1 or better, to get the propagation of extra
# columns.
if (Starlink::Versions::starversion_lt('ccdpack', '4.0-1')) {
    die "We have CCDPack version "
      .Starlink::Versions::starversion_string('ccdpack')
      .", but we need 4.0-1 or better\n";
}
# And we need ATOOLS 1.1-2 or better, to get asttrann
if (Starlink::Versions::starversion_lt('atools', '1.3')) {
    die "We have ATOOLS version "
      .Starlink::Versions::starversion_string('atools')
      .", but we need 1.3 or better\n";
}

# Useful values
my $d2r = 57.295779513082320876798155; # degrees to radians (quite accurately)



# Examine the argument list and options
my %optionset = ();
GetOptions (\%optionset,
	    "keep!",		# keep temporary files?
	    "temp=s",		# name of temporary directory
	    "timeout=i",	# monolith timeout
	    "messages!",	# show messages from monoliths?
	    "maxobj=i",		# maximum number of objects to process
	    "catalogue=s",	# SkyCat name of online catalogue to use 
	    "skycatconfig=s")	# SkyCat configuration file
  || die "Can't parse options\n";

$keeptemps = $optionset{keep} if defined ($optionset{keep});
$tempDir   = $optionset{temp} if defined ($optionset{temp});
$monolithTimeout = $optionset{timeout} if defined ($optionset{timeout});
$monolithMessages = $optionset{messages} if defined ($optionset{messages});
$maxobj = $optionset{maxobj} if defined ($optionset{maxobj});
$esocatname = $optionset{catalogue} if defined ($optionset{catalogue});
$skycatconfig = $optionset{skycatconfig} if defined($optionset{skycatconfig});

reuse_files() if $tempDir;

($#ARGV == 0) || die "Must have exactly one NDF argument\n";

$NDF = $ARGV[0];



# Generate a temporary file name
unless (defined ($tempDir)) {
    $tempDir = "$ENV{AUTOASTROM_TEMPDIR}/autoastrom-$$";
    mkdir ($tempDir, 0755) 
      || die "Can't create directory $tempDir, for some reason. Giving up!\n";
}
print STDERR "NDF=$NDF  progpath=$progpath  tempDir=$tempDir\n";

print STDERR "keeptemps=$keeptemps  tempDir=",
  (defined($tempDir) ? $tempDir : "<undef>"),
  "  NDF=", (defined($NDF) ? $NDF : "<undef>"),
  "\n";

defined($NDF) || die "Usage: $Usage\n";





# Start things up, set the timeout to be 30 seconds, and turn off all
# but error messages by setting $ams->messages(0) (don't do this yet).
my $ams = new Starlink::AMS::Init(1);
$ams->timeout($monolithTimeout);
$ams->messages($monolithMessages);


# Define the CONVERT environment variables.
#
# We don't actually have to start anything up here.  CONVERT setup
# (for our present purposes) consists of specifying a number of
# environment variables to control how the NDF library recognises
# foreign formats and how it invokes the converters.  See
# $CONVERT_DIR/convert.csh
#
# We don't need the full panoply of FITS file extensions, since the
# only FITS files we'll be wanting to read will be FITS-WCS files
# created by this application.  The following is more than enough.
$ENV{NDF_FORMATS_IN} = 'FITS(.fits),FITS(.fit),FITS(.fts),FITS(.FITS),FITS(.FIT),FITS(.FTS)';
$ENV{NDF_FROM_FITS} = "$ENV{CONVERT_DIR}/convertndf from '^fmt' '^dir' '^name' '^type' '^fxs' '^ndf'";
$ENV{NDF_TEMP_FITS} = 'temp_fits_^name^fxscl';

# temp NDF debugging switch
$ENV{NDF_SHCVT} = 1;
#system ('env > /tmp/autoastrom-env-dump');


my %helpers = ();
# I don't understand why, but all of these monoliths need to be
# started up here rather than in the individual subroutines.  If we
# don't do this, we get crashes.
#
# Start up KAPPA
my $Kappa = new Starlink::AMS::Task
  ("kappa_mon_$$", "$ENV{KAPPA_DIR}/kappa_mon");
$Kappa->contactw || die "Error launching Kappa - timeout";
$helpers{kappa} = $Kappa;

# ... and KAPPA's NDFPACK
my $NDFPack = new Starlink::AMS::Task
  ("ndfpack_mon_$$", "$ENV{KAPPA_DIR}/ndfpack_mon");
$NDFPack->contactw || die "Error launching NDFPack - timeout";
$helpers{ndfpack} = $NDFPack;

# ... and CCDPack
my $CCDPack = new Starlink::AMS::Task
  ("ccd_mon_$$", "$ENV{CCDPACK_DIR}/ccdpack_reg");
$CCDPack->contactw || die "Error launching CCDPack - timeout";
$helpers{ccdpack} = $CCDPack;

# ... and SExtractor
#
# SExtractor is called with a custom config file which is set up to
# put output in the place pointed to by the AUTOASTROMTEMPCATALOGUE
# environment variable.
$ENV{AUTOASTROMTEMPCATALOGUE} = "$tempDir/extractor-temp";
push (@TEMPFILES, $ENV{AUTOASTROMTEMPCATALOGUE});
my $extractor = new Starlink::AMS::Task
  ("extractor_mon_$$", "$ENV{EXTRACTOR_DIR}/extractor");
$extractor->contactw  || die "Error launching extractor -- timeout";
$helpers{extractor} = $extractor;

# ... and ATOOLS
my $atools = new Starlink::AMS::Task
  ("atools_mon_$$", "$ENV{ATOOLS_DIR}/atools_mon");
$atools->contactw || die "Error launching atools -- timeout";
$helpers{atools} = $atools;



# Obtain the PIXEL coordinates of the corners of the NDF image, and
# the WCS information from the WCS component of the NDF.  Returns a
# hash with {date}, {x0}, {y0}, {x1}, {y1}, {wcs}, {hassky})
my $NDFinfo = ndf_info ($NDF, \%helpers, $tempDir);

if (! defined($NDFinfo->{wcs})) {
    print STDERR "Unable to find any WCS information in NDF $NDF.  Giving up!\n";
    exit 1;
}
if (! $NDFinfo->{hassky}) {
    print STDERR "The NDF $NDF does not appear to have a SKY domain.\nYou need to do at least an approximate calibration first\n";
    exit 1;
}

# Find the moggy program.  Searching for it like this gives us a
# little more flexibility in installation locations (and means that
# the system will work for testing in its build directory).
my $moggylocation = undef;
foreach my $loc ("$ENV{AUTOASTROM_DIR}/moggy", 
	      "$ENV{AUTOASTROM_DIR}/moggy/moggy") {
    if (-f $loc && -x $loc) {
	$moggylocation = $loc;
	last;
    }
}
defined ($moggylocation)
  || die "Can't locate moggy program\n";
print STDERR "moggylocation=$moggylocation\n";
my $moggy = Moggy->new($esocatname, $skycatconfig, $moggylocation);
$helpers{moggy} = $moggy;
#$moggy = Moggy->new ('dummy@home',
#		     'file:///home/norman/s/src/autoastrom/w/autoastrom/moggy/t/local.config');

#$moggy->debug("cataloguehandler");

# Obtain a catalogue covering the same part of the sky as the NDF.  We
# don't have to assume here that the projection pole is in the centre
# of the image, since ASTROM isn't fazed if it it's elsewhere.
my $CAT = get_catalogue ($moggy, %$NDFinfo, $maxobj, $tempDir);

# ...and write an NDF which has the columns (number, ra/rad, dec/rad, ra,
# dec).  That is, copy the $PIXcat file, duplicating columns (3,4)
# into columns (1,2) and converting them to radians (required for asttrann).
my @catdata = txt2arr ($CAT);
($#catdata >= 0)
  || die "Catalogue between corners (".$NDFinfo->{x0}.','.$NDFinfo->{y0}.") and (".$NDFinfo->{x1}.','.$NDFinfo->{y1}.") has no entries!\n";
my $r;
foreach $r (@catdata) {
    $r->[1] = $r->[3]/$d2r;
    $r->[2] = $r->[4]/$d2r;
}
my $CATNDF = twodarray2ndf (@catdata, $CAT)
  || die "Can't create NDF $CAT\n";
push (@TEMPFILES, "$CATNDF.sdf");
print STDERR "CAT=$CAT  CATNDF=$CATNDF\n";



# Extract a catalogue of objects from the NDF
my $CCDcat = extract_objects (\%helpers, $NDF, $maxobj, $tempDir);



print STDERR "get_catalogue    returned $CAT\n";
print STDERR "extract_objects  returned $CCDcat\n";


# We run ASTROM $iterationsleft times.  We may stop the loop from
# within by setting $iterationsleft to zero.
#
# How do we decide how many iterations is enough?  If we do the
# iteration one more time after we get a 9-parameter fit, we do get an
# improvement in both rrms and nstars, as well as a couple of arcmin
# shift in the plate centre.
my $iterationsleft = 5;

# The following hold the state between iterations
my $findofferrs =     7.0;
my $lastfindofferrs = 0;
my $findoffnmatches = 0;
my $findoffinfile =   $CAT;
my $findoffndfs =     'X';	# dummy initial value
my $findofffits =     'X';	# dummy initial value

# We have rather heuristic decisions to make, regarding the fit we
# want ASTROM to attempt.  The ASTROM documentation recommends
# attempting the fit for tilt and distortion only when there are at
# least 10 reference stars, and we have the observation data `Obs',
# `Met' and `Col'.  If we do not have this, we should stick with a
# 6-parameter fit.  If we do have this, we should always ask for the
# full 9-parameter fit _unless_ this full fit failed last time, in
# which case we should use the 7-parameter fit, and have another try
# at the 9-parameter fit next time round.  Experience with CCDs which
# are mounted significantly eccentrically (the projection pole off the
# plate) suggests that the 8-parameter fit is less robust than the
# 7-parameter one.
#
# The decision about which fit to make is done in generate_astrom,
# controlled by its parameter maxnterms (set equal to variable
# $astromnterms here), which contains the _maximum_ number of terms to
# use.  This will only ever have the values 7 or 9 (and we switch
# between these at the end of the loop) -- if the actual number of
# parameters is to be lowered to 6, that'll be done within
# generate_astrom.
my @astromnterms = (7, 9);
my $ntermidx = $#astromnterms;	# Index into @astromnterms

my %lastastrom;			# Hash holding the results of the last
                                # (successful) ASTROM run.

my $findoffmaxdisp;
# Make $findoffmaxdisp a quarter of the shorter side of the image.
# This doesn't have to be exact, but it's better than not specifying
# it at all.  Successive iterations decrease this.
if (  abs($NDFinfo->{x1} - $NDFinfo->{x0})
    < abs($NDFinfo->{y1} - $NDFinfo->{y0})) {
    $findoffmaxdisp = abs($NDFinfo->{x1} - $NDFinfo->{x0});
} else {
    $findoffmaxdisp = abs($NDFinfo->{y1} - $NDFinfo->{y0});
}

my $iterno = 0;
while ($iterno < $iterationsleft) {
    print STDERR "-------------------------\nStarting iteration $iterno...\n";

    my $tfile = sprintf ("%s/%02d", $tempDir, $iterno);

    # Invoke FINDOFF, returning the names of the two output files
    my ($ccdlist, $catlist, $matchworked)
      = match_positions ($CCDPack,
			 $CCDcat, $findoffinfile,
			 {error => $findofferrs,
			  maxdisp => $findoffmaxdisp},
			 $tfile);

    if (! $matchworked) {
	# The match didn't work.  That's most commonly because the
	# value of error that we estimated was too low.  Try it again,
	# using the last value which worked, if any.
	if ($iterno == 0) {
	    # No previous one.
	    $findofferrs *= 1.5; # guess
	} else {
	    # Use the last one (which presumably worked)
	    $findofferrs = $lastfindofferrs;
	}
	($ccdlist, $catlist, $matchworked)
	  = match_positions ($CCDPack,
			     $CCDcat, $findoffinfile,
			     {error => $findofferrs,
			      maxdisp => $findoffmaxdisp},
			     $tfile);
    }
	    
    if (! $matchworked) {
	print "I can't match position lists.  Giving up.\n";
	$iterationsleft = 0;	# Redundant (given `last' below), but neat.
	last;			# JUMP OUT of the loop
    }
    $lastfindofferrs = $findofferrs;

    $findoffmaxdisp /= 2.0;	# ...for next time

    print STDERR "match_positions returned...\n\t$ccdlist\n\t$catlist\n";


    # Generate the ASTROM input file.  Returns hash with keys
    # {filename}, {nmatches}, {quality} and {findofferror}.
    my $astromparams = generate_astrom ({CCDin => $ccdlist,
					 catalogue => $catlist,
					 findofferror => $findofferrs,
					 helpers => \%helpers,
					 NDFinfo => $NDFinfo,
					 astrom => \%lastastrom,
					 maxnterms => $astromnterms[$ntermidx],
					 tempfn => $tfile});
    defined($astromparams) || die "generate_astrom failed\n";

    printf STDERR ("generate_astrom (\n\tccdlist=%s\n\tcatlist=%s\n\tfofferr=%f\n\tastromnterms=%d\n\ttfile=%s)\n    returned:\n",
		   $ccdlist,$catlist,$findofferrs,
		   $astromnterms[$ntermidx],$tfile);
    foreach my $k (sort keys %$astromparams) {
	printf STDERR "\t%s => %s\n", $k, $astromparams->{$k};
    }

    print STDERR "run_astrom (",$astromparams->{filename},", $tfile)...\n";
    my @fitdetails = run_astrom ($astromparams->{filename}, $tfile);
    @fitdetails || die "run_astrom failed\n";

    # Find the FITS files generated by ASTROM.  The highest-numbered
    # one is the best fit.  The WCS information in FITS files
    # represents a transformation from pixel to sky coordinates or, in
    # AST terms, the PIXEL domain is the BASE domain and the SKY is
    # the CURRENT domain, so to map sky coordinates to pixel ones,  the
    # AST transformation has to be done in the reverse direction.
    print STDERR "ASTROM fits, round $iterno:\n\tn nterms         centre            FITS-WCS\n";
    foreach my $k (0..$#fitdetails) {
	if ($fitdetails[$k]->{STATUS}) {
	    printf STDERR ("\t%d   %2d  %.12s %.13s %s\n",
			   $k,
			   $fitdetails[$k]->{nterms},
			   $fitdetails[$k]->{rasex},
			   $fitdetails[$k]->{decsex},
			   (length($fitdetails[$k]->{wcs}) > 35)
			   ? "...".substr($fitdetails[$k]->{wcs},-32)
			   : $fitdetails[$k]);
	} else {
	    printf STDERR "\t%d    NO FIT\n", $k;
	}
    }

    # Check whether the highest-order fit failed
    my $bestfitfailed = 0;
    # %lastastrom = %{$fitdetails[$#fitdetails]};
    for (my $k=$#fitdetails; $k>=0; $k--) {
	%lastastrom = %{$fitdetails[$k]};
	if ($lastastrom{STATUS}) {
	    # This fit was OK
	    printf STDERR ("ASTROM: best fit: %d of %d\n",
			   $k+1, $#fitdetails+1);
	    foreach my $kw (sort keys %lastastrom) {
		printf STDERR "\t%s => %s\n", $kw, $lastastrom{$kw};
	    }
	    last;
	} else {
	    printf STDERR ("ASTROM: Fit %d of %d rejected",
			   $k+1, $#fitdetails+1);
	    print STDERR " (",$lastastrom{nterms}, " terms)"
	      if (defined($lastastrom{nterms}));
	    print STDERR "\n";
	    $bestfitfailed = 1;
	}
    }

    # At least one of the fits worked, didn't it?
    $lastastrom{STATUS} || croak "ASTROM failed to make any fits at all!\n";

    $findoffnmatches = $astromparams->{nmatches};

    # Store the (best of the) WCS information obtained by ASTROM.
    $findofffits = $lastastrom{wcs};

    # Generate the name of an NDF to receive the output from the
    # coordinate transformation.  This is to be the coordinates of the
    # catalogue objects, in the pixel domain specified by the WCS
    # information obtained by ASTROM (ie, $lastastrom{wcs}).
    $findoffndfs = "$tfile-tran";

    # Produce the ASTTRANN argument list.  First, the astrometry...
    my $asttrannarg = "this=$findofffits";
    # then the input and output NDFs...
    $asttrannarg .= " in=$CATNDF out=$findoffndfs";
    # reverse transformation (ie, SKY to PIXEL domain) of columns 2 and 3...
    $asttrannarg .= " forward=false incols=[2,3]";

    # Call ASTTRANN
    print STDERR "Calling asttrann\n";
    foreach my $e (split(' ',$asttrannarg)) {
	print STDERR "\t$e\n";
    }
    my $status = $atools->obeyw ("asttrann", $asttrannarg);
    ($status == &Starlink::ADAM::DTASK__ACTCOMPLETE)
      || croak "Error getting atools/asttrann";

    push (@TEMPFILES, "$findoffndfs.sdf");

    # Now turn $findoffndfs[$iterno+1] (the best-so-far pixel coordinates
    # of the reference stars) into a text input file for FINDOFF.
    $findoffinfile = ndf2txt ($findoffndfs);
    push (@TEMPFILES, $findoffinfile);

    # We want to estimate the best value of findoff:error for the next
    # round.  Use the value returned from generate_astrom.
    $findofferrs = $astromparams->{findofferror};

    # What sort of ASTROM fit do we want to try next time?
    if ($bestfitfailed) {
	# be less adventurous next time
	$ntermidx-- if ($ntermidx > 0);
    } else {
	# be more adventurous next time
	$ntermidx++ if ($ntermidx < $#astromnterms);
    }


    # Try a fit with one
    # more term than the best last time.
#    if ($ntermsidx > 0) {
#	$ntermsidx--;		# one better
#    } elsif ($nt
#    if ($astromnterms[$iterno] < 9) {
#	$astromnterms[$iterno+1] = $astromnterms[$iterno] + 1;
#    } else {
#	$astromnterms[$iterno+1] = $astromnterms[$iterno];
#    }

} continue {
    $iterno++;
}

print STDERR "Finished???\n";
#print STDERR "findofferrs     : @findofferrs\n";
#print STDERR "findoffnmatches : @findoffnmatches\n";
#print STDERR "findoffinfile   : @findoffinfile\n";
#print STDERR "findoffndfs     : @findoffndfs\n";
#print STDERR "findofffits     : @findofffits\n";

print STDERR "Temp files:\n";
push (@TEMPFILES, get_temp_files());
my $i;
foreach $i (@TEMPFILES) {
    print STDERR "    $i\n";
}

exit 0;


