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


$Usage = "$0 [--keep] [--temp=dir] [--noregenerate] <NDF-file>";
($progpath = $0) =~ s+/[^/]*$++;

$keeptemps = 1;			# Set by default, at present
$NDF = undef;
$tempDir = undef;		# Allow this to be forced on the
                                # commandline.  This is presently
                                # mostly for debugging, but could be
                                # used in some production modes to
                                # redo some processing.
#$noregenerate = 0;		# If true, don't regenerate
                                # preexisting files (probably only
                                # used in conjunction with --temp to
                                # redo some processing).

# Global variables
@TEMPFILES = ( );

# Maximum number of objects to manipulate.  Guess number!
$maxobj = 500;


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
$ENV{ASTROM_DIR} = "$ENV{AUTOASTROM_DIR}/astrom"
  unless defined $ENV{ASTROM_DIR};
$ENV{CONVERT_DIR} = "$ENV{STARLINK}/bin/convert"
  unless defined $ENV{CONVERT_DIR};



# Include the ADAM interface modules, and version-checking module
use Starlink::AMS::Init;
use Starlink::AMS::Task;
use Starlink::Versions;
# Include the NDF modules, for the fits_read_header function
use NDF;



# Include the Moggy modules, for querying catalogues
# FOR TESTING ONLY, INCLUDE LIB DIR
use lib "$ENV{AA}";
use moggy::Moggy;


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
if (Starlink::Versions::starversion_lt('atools', '1.1-2')) {
    die "We have ATOOLS version "
      .Starlink::Versions::starversion_string('atools')
      .", but we need 1.1-2 or better\n";
}

# Useful values
$d2r = 57.295779513082320876798155; # degrees to radians (quite accurately)


# Examine the argument list and options
foreach $arg (@ARGV) {
    $arg eq '--keep' && 	do { $keeptemps = 1; next; };
    $arg =~ /^--temp=(.*)/ &&	do { $tempDir = $1; reuse_files(); next; };
    $arg =~ /^-/ &&		do { die "Usage: $Usage\n"; next; };
    if (defined($NDF)) {
	# Only allow a single NDF to be processed at once.
	die "Usage: $Usage\n";
    } else {
	# What NDF file should we be processing.  In future, we'll
	# want to get approximate pointing information from the user,
	# but at present, just assume that the NDF file with the CCD
	# images in it already has a WCS component.
	$NDF = $arg;
    }
}


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
$ams = new Starlink::AMS::Init(1);
$ams->timeout(30);
$ams->messages(1);


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
system ('env > /tmp/autoastrom-env-dump');


# I don't understand why, but all of these monoliths need to be
# started up here rather than in the individual subroutines.  If we
# don't do this, we get crashes.
#
# Start up KAPPA
$Kappa = new Starlink::AMS::Task
  ("kappa_mon_$$", "$ENV{KAPPA_DIR}/kappa_mon");
$Kappa->contactw || die "Error launching Kappa - timeout";

# ... and KAPPA's NDFPACK
$NDFPack = new Starlink::AMS::Task
  ("ndfpack_mon_$$", "$ENV{KAPPA_DIR}/ndfpack_mon");
$NDFPack->contactw || die "Error launching NDFPack - timeout";

# ... and CCDPack
$CCDPack = new Starlink::AMS::Task
  ("ccd_mon_$$", "$ENV{CCDPACK_DIR}/ccdpack_reg");
$CCDPack->contactw || die "Error launching CCDPack - timeout";

# ... and SExtractor
#
# SExtractor is called with a custom config file which is set up to
# put output in the place pointed to by the AUTOASTROMTEMPCATALOGUE
# environment variable.
$ENV{AUTOASTROMTEMPCATALOGUE} = "$tempDir/extractor-temp";
push (@TEMPFILES, $ENV{AUTOASTROMTEMPCATALOGUE});
$extractor = new Starlink::AMS::Task
  ("extractor_mon_$$", "$ENV{EXTRACTOR_DIR}/extractor");
$extractor->contactw  || die "Error launching extractor -- timeout";

# ... and ATOOLS
$atools = new Starlink::AMS::Task
  ("atools_mon_$$", "$ENV{ATOOLS_DIR}/atools_mon");
$atools->contactw || die "Error launching atools -- timeout";



# Obtain the PIXEL coordinates of the corners of the NDF image, and
# the WCS information from the WCS component of the NDF.
#($date, $wcsref, $NDFref)
@NDFinfo = ndf_info ($NDF, $Kappa, $NDFPack, $tempDir);


#$moggy = Moggy->new('usno@eso');
$moggy = Moggy->new ('dummy@home',
		     'file:///home/norman/s/src/autoastrom/w/autoastrom/moggy/t/local.config');
		     #'file:///scratch/goedel/norman/playpen/test.config');

# Obtain a catalogue covering the same part of the sky as the NDF.
$CAT = get_catalogue ($moggy, @NDFinfo, $maxobj, $tempDir);

# ...and write an NDF which has the columns (number, ra/rad, dec/rad, ra,
# dec).  That is, copy the $PIXcat file, duplicating columns (3,4)
# into columns (1,2) and converting them to radians (required for asttrann).
@catdata = txt2arr ($CAT);
foreach $r (@catdata) {
    $r->[1] = $r->[3]/$d2r;
    $r->[2] = $r->[4]/$d2r;
}
$CATNDF = twodarray2ndf (@catdata, $CAT)
  || die "Can't create NDF $CAT\n";
push (@TEMPFILES, "$CATNDF.sdf");
print STDERR "CAT=$CAT  CATNDF=$CATNDF\n";



# Extract a catalogue of objects from the NDF
$CCDcat = extract_objects ($extractor, $NDF, $maxobj, $tempDir);

## ...and write it to an NDF
#$CCDNDF = txt2ndf ($CCDcat);



print STDERR "get_catalogue    returned $CAT\n";
print STDERR "extract_objects  returned $CCDcat\n";


my @findofferrs =     (7, 0, 0);
my @findoffnmatches = (0);
my @findoffinfile =   ($CAT);
my @findoffndfs =     ('X');
my @findofffits =     ('X');
for ($i=0; $i<$#findofferrs; $i++) {

    my $fofferr = $findofferrs[$i];
    my $tfile = sprintf ("%s/%02d", $tempDir, $i);

    # Invoke FINDOFF, returning the names of the two output files
    ($ccdlist, $catlist)
      = match_positions ($CCDPack,
			 $CCDcat, $findoffinfile[$i],
			 $fofferr, $tfile);

    print STDERR "match_positions  returned ($ccdlist,$catlist)\n";


    # Generate the ASTROM input file.
    # Returns (astrom file, nmatches, quality).
    @astrominput = generate_astrom ($ccdlist, $catlist, $fofferr,
				    $moggy, @NDFinfo,
				    $atools,
				    ($i==0 ? undef : $findofffits[$i]),
				    $tfile);
    @astrominput || die "generate_astrom failed\n";

    print STDERR "generate_astrom ($ccdlist,$catlist,$fofferr,$tfile) returned ($astrominput[0], $astrominput[1], $astrominput[2])\n";

    my @fitdetails = run_astrom ($astrominput[0], $tfile);
    @fitdetails || die "run_astrom failed\n";

    # Find the FITS files generated by ASTROM.  The highest-numbered
    # one is the best fit.  The WCS information in FITS files
    # represents a transformation from pixel to sky coordinates or, in
    # AST terms, the PIXEL domain is the BASE domain and the SKY is
    # the CURRENT domain, so to map sky coordinates to pixel ones,  the
    # AST transformation has to be done in the reverse direction.
    print STDERR "ASTROM returned ", $#fitdetails + 1, " fit details\n";
    my @bestfit = @{$fitdetails[$#fitdetails]};
    print STDERR "ASTROM best fit: @bestfit\n";
    $findoffnmatches[$i] = $astrominput[1];

    $findoffndfs[$i+1] = "$tfile-tran";
    my $asttrannarg = "mapping=$bestfit[0] incoord=$CATNDF outcoord=$findoffndfs[$i+1] forward=false incols=[2,3]";
    print STDERR "Calling: asttrann $asttrannarg\n";
    my $status = $atools->obeyw ("asttrann", $asttrannarg);
    ($status == &Starlink::ADAM::DTASK__ACTCOMPLETE)
      || die "Error getting atools/asttrann";

    push (@TEMPFILES, "$findoffndfs[$i+1].sdf");

    $findofffits[$i+1] = $bestfit[0];

    # Now turn $tfile into a text input file for FINDOFF.  Set the
    # errors for the next round to 1.5*sqrt($bestfit[2]/$bestfit[1])
    # (that is, the RMS pixel residual, times a safety factor).  And
    # go round the loop again.

    $findoffinfile[$i+1] = ndf2txt ($findoffndfs[$i+1]);
    push (@TEMPFILES, $findoffinfile[$i+1]);

    $findofferrs[$i+1] = 1.5*sqrt($bestfit[2]/$bestfit[1]);

#    push (@astromfiles, $astrominput[0]);
#    push (@findoffnmatches, $astrominput[1]);
#
#    if ($i < $#findofferrs) {
#	# Due to go around the loop again.  Estimate another
#	# findoff:error to take a punt at.  Hokey algorithm, but it's a
#	# type of crude maximum-likelihood fit.  The factor of 1.3 is
#	# plucked from the air.
#	$findofferrs[$i+1] = exp(log($findofferrs[$i]
#				     - $astrominput[2]*log(1.3)));
#    }
}

print STDERR "Finished???\n";
print STDERR "findofferrs     : @findofferrs\n";
print STDERR "findoffnmatches : @findoffnmatches\n";
print STDERR "findoffinfile   : @findoffinfile\n";
print STDERR "findoffndfs     : @findoffndfs\n";
print STDERR "findofffits     : @findofffits\n";

print STDERR "Temp files:\n";
push (@TEMPFILES, get_temp_files());
for ($i = 0; $i <= $#TEMPFILES; $i++) {
    print STDERR "    $i: $TEMPFILES[$i]\n";
}

exit 0;


