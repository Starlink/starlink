#
# Subroutines for autoastrom.pl
#
# Broken into a separate file (a) in case I want to reuse them, and
# (b) so that I can run regression tests on them.
#
# $Id$

package autoastrom;
use Exporter;
@ISA = qw(Exporter);

@EXPORT = qw( extract_objects ndf_info get_catalogue match_positions
	      generate_astrom run_astrom
	      twodarray2ndf ndf2twodarray txt2arr txt2ndf ndf2txt
	      reuse_files get_temp_files make_pseudo_fits
	      verbosity wmessage check_kwd_list );

@EXPORT_OK = (@EXPORT,
	      'deg2sex',
	      'sex2deg',
	      'ymd2je',
	      'ymd2jd',
	      'jd2je',
	      'qchi',
	      'canonicalise_ndfname',
	      'get_dates',
	      'parse_fits_date',
	     );


use strict;
use Carp;

# Include the NDF modules, for the fits_read_header function
use NDF;


# Declare subroutines with prototypes
sub extract_objects ($$$$);
sub ndf_info ($$$$);
sub get_catalogue ($\%$$);
sub match_positions ($$$$$);
sub generate_astrom ($);
sub run_astrom ($$);
sub deg2sex ($$;$);
sub sex2deg ($$);
sub ymd2je ($$$);
sub ymd2jd ($$$);
sub jd2je ($);
sub qchi ($$);
sub canonicalise_ndfname ($);
sub twodarray2ndf (\@$);
sub ndf2twodarray ($);
sub txt2arr ($);
sub txt2ndf ($;$);
sub ndf2txt ($;$);
sub reuse_files (;$);
sub get_temp_files ();
sub get_dates ($$$);
sub parse_fits_date ($);
sub invP ($);
sub make_pseudo_fits (\%\%);
sub check_kwd_list ($$);
sub verbosity ($);
sub wmessage ($$);

my $noregenerate = 0;
my @tempfiles = ();
my $verbose = 0;	  

# Useful values
my $d2r = 57.295779513082320876798155; # degrees to radians (quite accurately)

sub extract_objects ($$$$) {
    my ($helpers, $ndfname, $maxobj, $tempdir) = @_;
    # Extract objects from the NDF file $ndfname, using $tempdir as a
    # path for temporary files, and returning a maximum of $maxobj
    # objects.
    #
    # Return a (reference to) hash containing: {filename}, filename of the
    # resulting catalogue; {numobj}, the number of objects matched;
    # {poserr}, std.dev. of the positions, in pixels; and {objsize},
    # the size scale of the objects, in pixels.  The file which is
    # returned should be suitable for input to FINDOFF.  On error,
    # return undef.

    my %rethash = ();

    my $extractor = $helpers->{extractor};
    my $catname = "$tempdir/ccdcat.munged";

    push (@tempfiles, $catname);

    # We need to tell SExtractor where to put the catalogue output.
    # The custom configuration file we use sets CATALOG_NAME to have
    # the value $AUTOASTROMTEMPCATALOGUE.  This should have been set
    # up already, but check it here just in case.
    unless (defined $ENV{AUTOASTROMTEMPCATALOGUE}) {
	$ENV{AUTOASTROMTEMPCATALOGUE} = "$tempdir/ccdcat.extractor";
	push (@tempfiles, $ENV{AUTOASTROMTEMPCATALOGUE});
    }
    my $tcat = $ENV{AUTOASTROMTEMPCATALOGUE};

    my $parlist = "image=$ndfname config=$ENV{AUTOASTROM_DIR}/extractor.config keywords=false";
    print STDERR "Calling EXTRACTOR: extractor $parlist\n" if $verbose;
    my $status = $extractor->obeyw ("extractor", $parlist);

    $status == &Starlink::ADAM::DTASK__ACTCOMPLETE
      || confess "Error running extractor";

    # Now convert the output to a form suitable for input to
    # FINDOFF. That means that the first three columns should be
    # `integer-identifier X Y'.  Columns:
    #
    #    int-id, X, Y, int-id, flux
    #
    # Columns after the first three are ignored by FINDOFF, but
    # propagated to the output.  Flux column is mostly for debugging.
    # Sort these by column 8, FLUX_MAX, and output at most $maxobj objects.
    open (CAT, $tcat) || return undef;
    my %cathash = ( );
    my $line;
    # Read headers
    my %colkey = ();
    while (defined($line = <CAT>)) {
	# Read the header lines output by extractor, which are of the
	# format `#  col-num  col-label'
	$line =~ /^\# *([0-9]*) *([A-Z0-9_]*)/ || last;
	$colkey{$2} = $1;
	printf STDERR ("EXTRACTOR header line: <%.25s...>: colkey{%s}=%s\n",
		       $line, $2, $colkey{$2}) if $verbose;
    }
    # Now $line contains the first non-header line
    my $NUMBERcol = $colkey{NUMBER} - 1;
    my $FLUXcol = $colkey{FLUX_MAX} - 1;
    my $Xcol = $colkey{X_IMAGE} - 1;
    my $Ycol = $colkey{Y_IMAGE} - 1;
    my $Acol = $colkey{A_IMAGE} - 1;
    my $Bcol = $colkey{B_IMAGE} - 1;
    my $ERRX2col = $colkey{ERRX2_IMAGE} - 1;
    my $ERRY2col = $colkey{ERRY2_IMAGE} - 1;
    (defined($NUMBERcol) && defined($FLUXcol)
     && defined($Xcol) && defined($Ycol)
     && defined($Acol) && defined($Bcol)
     && defined($ERRX2col) && defined($ERRY2col))
      || wmessage ('fatal', "EXTRACTOR output $tcat malformed!");
    my $nobj = 0;
    my $objrad = 0.0;
    my $objposerr = 0.0;
    do {
	chomp $line;
	next if ($line !~ /^ *[0-9]/);
	my @l = split (' ', $line);
	$cathash{$l[$FLUXcol]} = sprintf ("%5d %12.3f %12.3f %5d %12.1f",
					  $l[$NUMBERcol], $l[$Xcol], $l[$Ycol],
					  $l[$NUMBERcol], $l[$FLUXcol]);
	$nobj++;
	# Calculate some statistics for the detected objects.  We
	# don't have to worry too much about the details here -- the
	# aim is to get a rough estimate of the position errors and
	# object sizes, so we can make reasonable estimates to pass on
	# to the object-matching routines.
	#
	# Use the A and B columns to calculate a scale
	# for the size of the detected object.  A and B are the sizes
	# of the ellipse semi-major and semi-minor axes, so the area
	# of the ellipse (=\pi AB) is the same as the area of a circle
	# with radius \sqrt{AB}.
	$objrad += sqrt($l[$Acol] * $l[$Bcol]);
	# Use the ERRX2 and ERRY2 columns (variances) to calculate an
	# estimate of the position error.  The standard deviation of
	# r=\sqrt{x^2+y^2} is \sqrt{ERRX2+ERRY2}.
	$objposerr += sqrt($l[$ERRX2col] + $l[$ERRY2col]);
    }
    while (defined($line = <CAT>));
    close (CAT);

    my @sortcat = sort {$b <=> $a} keys(%cathash);

    $rethash{filename} = $catname;
    $rethash{numobj} = $nobj;
    $rethash{poserr} = $objposerr/$nobj;
    $rethash{objsize} = $objrad/$nobj;

    open (CAT, ">$catname") || return undef;
    print CAT "# SExtractor catalogue, munged by $0\n";
    foreach my $k (keys (%rethash)) {
	print CAT "## $k ", $rethash{$k}, "\n";
    }
    while ($maxobj > 0 && $#sortcat >= 0) {
	#print CAT "# $maxobj  $sortcat[0]\n";
	print CAT $cathash{$sortcat[0]}, "\n";
	$maxobj--;
	shift (@sortcat);
    }
    close (CAT);

    #return $catname;
    return \%rethash;
}


# Extract information from the NDF.  This sets the WCS domain to be
# SKY, and returns the original domain.  It also finds the SKY
# coordinates of the bounds of the NDF.
#
# Arguments: NDF: the NDF to be examined; helpers: a hash pointing to
# the helper programs; obsdata: the obsdata hash, giving the
# parameters specified to the --obsdata option; tempdir: the temporary
# directory to be used.
#
# Return a hash comprising
#
#    - {date}: the observation date;
#
#    - {dim1}, {dim2} are the dimensions of the image;
#
#    - {ast}: a reference to an array containing the lines of the AST
#    WCS information extracted from the NDF, or undef if there is none
#    available;
#
#    - {fits}: a reference to an array containing the lines of any
#    FITS extension, or undef is none is available;
#
#    - {hassky}: true if that WCS information does contain a SKY
#    domain
#
#    - {astromtime}, {astromobs}, {astrommet}, {astromcol}: suitable
#    entries for the corresponding ASTROM observation data, if it's
#    possible to work out suitable values from the NDF.  The entries
#    {astromtimecomment} (etc.) give further details.
#
# On error, return the same array, but with undef for those
# components of it which cannot be obtained.
#
sub ndf_info ($$$$) {
    my ($NDF, $helpers, $obsdata, $tempdir) = @_;

    #my $Kappa = $helpers->{kappa};
    my $NDFPack = $helpers->{ndfpack};

    my ($origdomain, @lboundpix, @uboundpix);
    my $okstatus = &Starlink::ADAM::SAI__OK;

    my %returnhash;		# returned

    my $status = $NDFPack->obeyw ("ndftrace", "$NDF quiet=true");
    ($status == &Starlink::ADAM::DTASK__ACTCOMPLETE)
      || confess "Error running ndftrace\n";

    my @dims;
    ($status, @dims) = $NDFPack->get ("ndftrace", "dims");
    ($status == $okstatus)
      || confess "Error getting ndftrace/dims\n";
    $returnhash{dim1} = $dims[0];
    $returnhash{dim2} = $dims[1];
    print STDERR "ndf_info: NDF=$NDF dim[0]=$dims[0], dim[1]=$dims[1]\n"
      if $verbose;

    # Extract the AST WCS information from the NDF
    my $tfile = "$tempdir/wcsshow";
    if ($noregenerate && -e $tfile) {
	print STDERR "Reusing $tfile...\n" if $verbose;
    } else {
	$status = $NDFPack->obeyw ("wcsshow", "ndf=$NDF logfile=$tfile quiet=true full=-1");
	($status == &Starlink::ADAM::DTASK__ACTCOMPLETE)
	  || confess "Error running wcsshow";

	push (@tempfiles, $tfile);
    }
    my @domainlist;
    my $isSkyDomain = 0;
    if (open (WCS, "<$tfile")) {
	my @astlines = ( );
	my $line;
	while (defined($line = <WCS>)) {
	    chomp $line;
	    $line =~ s/^ *//;
	    if (!$isSkyDomain
		&& ($line =~ /domain *= *"?sky"?/i || $line =~ /begin  *skyframe/i)) {
		$isSkyDomain = 1;
		print STDERR "SKY domain exists\n" if $verbose;
	    }
	    push (@astlines, $line);
	}
	close (WCS);
	$returnhash{ast} = \@astlines;
    } else {
	print STDERR "ndf_info: NDF $NDF does not appear to have a WCS component\n"
	  if $verbose;
    }
    $returnhash{hassky} = $isSkyDomain;

    # Now extract any FITS extension.  If we only wanted random-access
    # to individual records, then we could get a hash of records more
    # directly using fits_read_header, but we additionally want to
    # extract the complete FITS header (so we can pass it back to the
    # caller), so read it in using HDS commands, and construct the hash
    # by hand.

    my ($indf,$fitsloc,$fitssize,@fits,$nfits);
    my $ndfstatus = &NDF::SAI__OK;

    # Begin an NDF context
    ndf_begin();
    ndf_find (NDF::DAT__ROOT(), $NDF, $indf, $ndfstatus);
    printf STDERR ("ndf_info: NDF=%s  indf=%d  ndfstatus=%d  OK=%d\n",
		   $NDF, $indf, $ndfstatus, &NDF::SAI__OK) if $verbose;
    ndf_xloc ($indf, 'FITS', 'READ', $fitsloc, $ndfstatus);
    dat_size ($fitsloc, $fitssize, $ndfstatus);
    print STDERR "ndf_info: fitssize=$fitssize  ndfstatus=$ndfstatus\n"
      if $verbose;
    dat_get1c ($fitsloc, $fitssize, @fits, $nfits, $ndfstatus);
    printf STDERR ("ndf_info: Obtained %d FITS records from NDF %s\n",
		   $#fits, $NDF) if $verbose;

    $returnhash{fits} = \@fits;

    my %fitshash = ();
    for (my $i=0; $i<=$#fits; $i++) {
	my ($keyword, $value, $comment) = fits_get_nth_item (@fits, $i);
	if (defined($keyword) && defined($value)) {
	    $fitshash{$keyword} = $value;
	}			# ignore `malformed' cards: blank, HISTORY, END
    }

    # Extract all the available date information from the NDF,
    # including from its FITS extension.
    my $ndfdates = get_dates ($indf, $helpers, \%fitshash);

    # Attempt to obtain observation for ASTROM observation records
    # ASTROM Time record
    if (defined($obsdata->{time})) {
	# Command-line specification of time.  We already know that
	# this is one of the three formats acceptable to ASTROM, so we
	# simply need to change separator characters to spaces.
	my $obstime = $obsdata->{time};
	$obstime =~ s/[^-0-9.]/ /;
	$returnhash{astromtime} = $obstime;
	$returnhash{astromtimecomment} = "command-line time";
    } elsif (defined($ndfdates->{nst})) {
	my @sthms = split (/[^0-9]+/,$ndfdates->{nst});
	$returnhash{astromtime} = sprintf ("%d %f",
					   $sthms[0],
					   $sthms[1]+($sthms[2]/60));
	$returnhash{astromtimecomment} = "NDF Sidereal time";
    } elsif (defined($ndfdates->{fst})) {
	my @sthms = split (/[^0-9]+/,$ndfdates->{fst});
	$returnhash{astromtime} = sprintf ("%d %f",
					   $sthms[0],
					   $sthms[1]+($sthms[2]/60));
	$returnhash{astromtimecomment} = "FITS ST";
    } elsif (defined($ndfdates->{nje})) {
	# Epoch is a valid Time record in our custom ASTROM, but not
	# (yet?) in standard ASTROM.
	$returnhash{astromtime} = $ndfdates->{nje};
	$returnhash{astromtimecomment} = "NDF JE";
    } elsif (defined($ndfdates->{fje})) {
	$returnhash{astromtime} = $ndfdates->{fje};
	$returnhash{astromtimecomment} = "FITS JE";
    } elsif (defined($obsdata->{ra})) {
	# We have no other information about the observation time.  In
	# this situation, the ASTROM documentation states that it is
	# legitimate to guess that the observation occurred near upper
	# culmination, so that we can set the ST to be the
	# plate-centre RA.  We don't have ready access to the
	# WCS information in the NDF, else we could use that.
	my @cmdra = split (/[^0-9]+/, $obsdata->{ra});
	$returnhash{astromtime} = "$cmdra[0] $cmdra[1]";
	$returnhash{astromtime} = "command-line RA";
    } elsif (defined($fitshash{CRVAL1})) {
	# Take it from the FITS extension

	# First, a quick sanity-check.
	if (defined($fitshash{CRVAL1}) && defined($fitshash{CRVAL2})
	    && defined($fitshash{CRTYPE1}) && defined($fitshash{CRTYPE2})) {
	    if ($fitshash{CRTYPE1} =~ /^RA/) {
		my $absdeg = abs($fitshash{CRVAL1});
		$returnhash{astromtime}
		    = sprintf ("%f %f", $absdeg,
			       ($fitshash{CRVAL1}-$absdeg)*60.0);
		$returnhash{astromtimecomment} = "FITS obs RA, CRVAL1";
	    } elsif ($fitshash{CRTYPE2} =~ /^RA/) {
		my $absdeg = abs($fitshash{CRVAL2});
		$returnhash{astromtime}
		    = sprintf ("%f %f", $absdeg,
			       ($fitshash{CRVAL2}-$absdeg)*60.0);
		$returnhash{astromtimecomment} = "FITS obs RA, CRVAL2";
	    }
	} else {
	    wmessage ('warning',
		  "Odd FITS file -- not all of CRVAL1,2 CRTYPE1,2 defined!");
	}
    }
    if ($verbose) {
	if (defined ($returnhash{astromtime})) {
	    printf STDERR ("ndf_info: ASTROM Time=%s from %s\n",
			   $returnhash{astromtime},
			   $returnhash{astromtimecomment});
	} else {
	    print STDERR "ndf_info: Can't work out ASTROM Time record\n";
	}
    }

    # ASTROM Obs record
    if (defined($obsdata->{obs})) {
	$returnhash{astromobs} = $obsdata->{obs};
	$returnhash{astromobscomment} = "command-line obs";
    } elsif (defined($fitshash{SLATEL})) {
	$returnhash{astromobs} = $fitshash{SLATEL};
	$returnhash{astromobscomment} = "FITS SLATEL";
	# We might alternatively/additionally use the OBSERVAT keyword which
	# (always?, sometimes?) contains an IRAF observatory
	# keyword. The list of these, though not the mappings to SLA
	# codes, is in the IRAF obsdb.dat file.  See
	# http://tdc-www.harvard.edu/iraf/rvsao/bcvcorr/obsdb.html
    } elsif (defined($fitshash{SITELAT}) && defined($fitshash{SITELONG})) {
	# Latitude and (east-)longitude in sexagesimal degrees (not a
	# standard keyword).  ASTROM requires latitude and east-positive
	# longitude, in degrees and arcmin.
	my @lat = split (/:/, $fitshash{SITELAT});
	my @long = split (/:/, $fitshash{SITELONG});
	my $ao = sprintf ("%d %d.%d  %d %d.%d",
			  $long[0], $long[1], $long[2]/60.0,
			  $lat[0], $lat[1], $lat[2]/60.0);
	$ao .= sprintf (" %f", $fitshash{HEIGHT})
	  if (defined($fitshash{HEIGHT}));
	$returnhash{astromobs} = $ao;
	$returnhash{astromobscomment} = "FITS SITELAT/SITELONG";
    } elsif (defined($fitshash{LATITUDE})
	     && defined($fitshash{LONGITUD})) {
	# Latitude and longitude in (decimal) degrees.  Note the
	# FITS LONGITUD keyword (not a standard one) appears to be
	# west-positive in the FITS files I've seen, though this
	# doesn't appear to be written down anywhere. ASTROM
	# requires the longitude to be east-positive.
	my $t = $fitshash{LONGITUD};
	my $ti = int($t);
	my $ao = sprintf ("%d %f", $ti, abs(($t-$ti)*60.0));
	$t = $fitshash{LATITUDE} * -1.0;
	$ti = int($t);
	$ao .= sprintf (" %d %f", $ti, abs(($t-$ti)*60.0));
	$ao .= sprintf (" %f", $fitshash{HEIGHT})
	  if (defined($fitshash{HEIGHT}));
	$returnhash{astromobs} = $ao;
	$returnhash{astromobscomment} = "FITS LATITUDE/LONGITUD";
    }
    if ($verbose) {
	if (defined($returnhash{astromobs})) {
	    printf STDERR ("ndf_info: ASTROM Obs=%s from %s\n",
			   $returnhash{astromobs},
			   $returnhash{astromobscomment});
	} else {
	    print STDERR "ndf_info: can't work out ASTROM Obs record\n";
	}
    }
    
    # ASTROM Met record
    if (defined($obsdata->{met})) {
	$returnhash{astrommet} = $obsdata->{met};
	$returnhash{astrommetcomment} = "command-line met";
    } elsif (defined($fitshash{TEMPTUBE})) {
	$returnhash{astrommet} = sprintf ("%f",
					  $fitshash{TEMPTUBE} + 273.15);
	$returnhash{astrommetcomment} = "FITS TEMPTUBE";
    }
    if ($verbose) {
	if (defined($returnhash{astrommet})) {
	    printf STDERR ("ndf_info: ASTROM Met=%s from %s\n",
			   $returnhash{astrommet},
			   $returnhash{astrommetcomment});
	} else {
	    print STDERR "ndf_info: can't work out ASTROM Met record\n";
	}
    }
    
    # ASTROM Colour record
    if (defined($obsdata->{col})) {
	$returnhash{astromcol} = $obsdata->{col};
	$returnhash{astromcolcomment} = "command-line col";
    } elsif (defined($fitshash{WFFBAND})) {
	my $t = uc($fitshash{WFFBAND});
	$t =~ s/ *//g;
	# Table from ASTROM documentation
	my %bandmap = ('U' => '365',
		       'B' => '415',
		       'V' => '575',
		       'R' => '675',
		       'I' => '800');
	
	$returnhash{astromcol} = $bandmap{$t} if (defined($bandmap{$t}));
	$returnhash{astromcolcomment} = "FITS WFFBAND";
    }
    if ($verbose) {
	if (defined($returnhash{astromcol})) {
	    printf STDERR ("ndf_info: ASTROM Col=%s from %s\n",
			   $returnhash{astromcol},
			   $returnhash{astromcolcomment});
	} else {
	    print STDERR "ndf_info: can't work out ASTROM Col record\n";
	}
    }
    
    my $obsdate;
    if (defined($ndfdates->{nje})) {
	$obsdate = $ndfdates->{nje};
	print STDERR "ndf_info: obsdate $obsdate from NDF history\n"
	  if $verbose;
    } elsif (defined($ndfdates->{fje})) {
	$obsdate = $ndfdates->{fje};
	print STDERR "ndf_info: obsdate $obsdate from FITS\n"
	  if $verbose;
    } else {
	my @now = localtime();
	$obsdate = ymd2je ($now[5]+1900, $now[4]+1, $now[3]);
	print STDERR "ndf_info: WARNING obsdate $obsdate taken to be NOW\n"
	  if $verbose;
    }

    # End the NDF context
    ndf_end ($ndfstatus);
    if ($ndfstatus != NDF::SAI__OK()) {
	print STDERR "get_date: NDF error (discarded)\n"
	  if $verbose;
    }

    $returnhash{date} = $obsdate;

    return \%returnhash;
}


# Given an NDF identifier (a pointer to an NDF file, opened by
# ndf_find), try to extract an observation date from it, or any FITS
# file included within it.  Return a hash containing as many as
# possible of the fields njd, fjd (julian date from NDF or FITS
# component), nje, fje (decimal date -- years AD, from NDF or FITS),
# today (decimal date), nst, fst (sidereal time of obs,
# hh:mm:ss.frac).
sub get_dates ($$$) {
    my ($indf, $helpers, $fitshash) = @_;
    my $okstatus = &NDF::SAI__OK;
    my $status = $okstatus;
    my $obsdate = undef;
    my %dates = ();

    # First look to see if this NDF has a history component.
    my $hashist;
    ndf_state ($indf, 'History', $hashist, $status);
    if ($status == $okstatus) {
	if ($hashist) {
	    # Yes, it has.  Now find the last history record
	    my ($nhist, $hrec);
	    ndf_hinfo ($indf, 'NRECORDS', 0, $nhist, $status);
	    print STDERR "NDF has $nhist history records\n"
	      if $verbose;
	    ndf_hinfo ($indf, 'DATE', $nhist, $hrec, $status);
	
	    my @dates = split ('[-: ]+', $hrec);
	    my @months = ('',
			  'jan','feb','mar','apr','may','jun',
			  'jul','aug','sep','oct','nov','dec');
	    my $mstr = lc($dates[1]);
	    my $mnum;
	    for ($mnum=1; $mnum<=12; $mnum++) {
		last if ($mstr eq $months[$mnum]);
	    }
	    ($mnum>=1 && $mnum<=12) || carp "get_date: Unknown month $mstr\n";

	    $dates{nje} = ymd2je ($dates[0], $mnum, $dates[2]);
	}
    }

    my $fitsdatesource = undef;
    if (defined($fitshash)) {
	if ($status != &NDF::SAI__OK) {
	    print STDERR "fits_read_header appeared to fail status=$status\n"
	      if $verbose;
	} elsif (defined($$fitshash{'JD'})) {
	    $fitsdatesource = 'JD';
	    $dates{fjd} = $$fitshash{'JD'};
	    # Convert JD to a decimal year.
	    # Julian epoch=J2000.0 + (JD - 2 451 545)/365.25
	    $dates{fje} = jd2je ($dates{fjd});
	    #$dates{fje} = 2000.0 + ($dates{fjd} - 2451545)/365.25;
	} elsif (defined($$fitshash{'DATE-OBS'})) {
	    $fitsdatesource = 'DATE-OBS';
	    $dates{fje} = parse_fits_date ($$fitshash{'DATE-OBS'});
	} elsif (defined($$fitshash{'EQUINOX'})) {
	    $fitsdatesource = 'EQUINOX';
	    # Supposed to be the equinox of the coordinate system, but
	    # is also a common default for the epoch of the
	    # observation.   In decimal years.
	    $dates{fje} = $$fitshash{'EQUINOX'};
	} elsif (defined($$fitshash{'EPOCH'})) {
	    $fitsdatesource = 'EPOCH';
	    # This was used with the same meaning as 'EQUINOX' in
	    # early FITS data sets, and is now defined as equivalent.
	    $dates{fje} = $$fitshash{'EPOCH'};
	} elsif (defined($$fitshash{'DATE'})) {
	    $fitsdatesource = 'DATE';
	    # This is actually the time the file was written.  Last-ditch.
	    $dates{fje} = parse_fits_date ($$fitshash{'DATE'});
	}
	printf STDERR ("fits_read_header{%s} = %s\n",
		       $fitsdatesource, $dates{fje})
	  if ($verbose && defined($fitsdatesource));

	if (defined($fitshash->{ST})) {
	    $dates{fst} = $fitshash->{ST};
	} elsif (defined($fitshash->{STSTART})) {
	    $dates{fst} = $fitshash->{STSTART};
	}
    }

    return \%dates;
}


# Invoke the moggy server to obtain a catalogue corresponding to the
# proffered bounds of the NDF.  Return the name of a file which is
# suitable for input into FINDOFF. 
#
# We will also want to call this with explicitly specified sky
# position and search size (for the case where the original NDF has no
# WCS information, and it has to be supplied on the command line).
# This should be easy if we supply the NDFinforef with ra, dec and
# radius fields.
sub get_catalogue ($\%$$) {
    my ($cat, $NDFinforef, $maxobj, $tempdir) = @_;

    my $mytempfile = "$tempdir/catalogue";

    if ($noregenerate && -e $mytempfile) {
	print STDERR "Reusing $mytempfile...\n"
	  if $verbose;
	return $mytempfile;
    }

    # Pass the WCS information to moggy, declaring that future points
    # will be specified in the GRID domain (that domain in which the
    # first pixel is centred at coordinate (1,1)).

    # XXX do everything in the GRID domain, not pixel
    #$cat->astinformation ('pixel', @{$NDFinforef->{wcs}});
    $cat->astinformation ('grid', @{$NDFinforef->{wcs}});

    # Determine the bounds of the image, and make a query of all the
    # objects sitting in a box with these points at opposite
    # corners. In fact, ask for a box somewhat larger than this (say,
    # 10% in linear dimension), anticipating some misalignment in the
    # initial astrometry.
    #
    # There's no need to make this margin of 10% configurable -- it
    # doesn't matter here if the projection pole is off the plate, as
    # long as the centre of _this_ plate is reasonably accurate.
    my $sizex = $NDFinforef->{dim1};
    my $sizey = $NDFinforef->{dim2};
    my $errorest = 0.1;
    $cat->searchtype('box');
    $cat->point     (-$errorest*$sizex,-$errorest*$sizey);
    $cat->otherpoint( (1.0+$errorest)*$sizex, (1.0+$errorest)*$sizey);

    # Get a decent number of points
    $cat->maxrow($maxobj);

    $cat->status_ok()
      || wmessage ('fatal',
		   "Can't set catalogue parameters ("
		   . $cat->current_statusmessage() . ")");

    $cat->query()
      || wmessage ('fatal', "Can't make catalogue query ("
		   . $cat->current_statusmessage() . ")");

#    $cat->status_ok()
#      || confess "Can't set catalogue parameters ("
#	. $cat->current_statusmessage() . ")\n";
#
#    $cat->query()
#      || confess "Can't make catalogue query ("
#	. $cat->current_statusmessage() . ")\n";

    printf STDERR ("get_catalogue: box(%f,%f)..(%f,%f) %d points\n",
		   $cat->point()->[0], $cat->point()->[1],
		   $cat->otherpoint()->[0], $cat->otherpoint()->[1],
		   $cat->resultnrows())
      if $verbose;

    # Now write the catalogue out to a file which can be used as input
    # to FINDOFF.  Columns:
    #
    #    int-id, X, Y, ra, dec, int-id, mag
    #
    # The extra columns are ignored by FINDOFF, but used by
    # generate_astrom.
    my $resref = $cat->result();
    my $nrows  = $cat->resultnrows();
    my $xcol   = $cat->resulthascolumn('x');
    my $ycol   = $cat->resulthascolumn('y');
    my $racol  = $cat->resulthascolumn('ra');
    my $deccol = $cat->resulthascolumn('dec');
    my $magcol = $cat->resulthascolumn('/mag/');
    ($xcol>=0 && $ycol>=0 && $racol>=0 && $deccol>=0)
      || confess "Can't find expected column ($xcol,$ycol,$racol,$deccol) in catalogue results\n";
    open (PIXCAT, ">$mytempfile")
      || confess "Can't open PIXCAT $mytempfile to write\n";
    push (@tempfiles, $mytempfile);
    print PIXCAT "# Catalogue generated from autoastrom.pl\n";
    my $i;
    for ($i=0; $i<$nrows; $i++) {
	printf PIXCAT "%5d %12.5f %12.5f %12.5f %12.5f %5d %12.5f\n",
	    $i,
	    $resref->[$i]->[$xcol], $resref->[$i]->[$ycol],
	    $resref->[$i]->[$racol], $resref->[$i]->[$deccol],
	    $i, ($magcol >= 0 ? $resref->[$i]->[$magcol] : 0);
    }
    close PIXCAT;

    return $mytempfile;
}



# Invoke FINDOFF to match the positions of the objects in the two
# input catalogues.  $tempfn is a filename prefix, to make temporary
# filenames out of, not a directory.
#
# Return an array containing the two files containing the FINDOFF
# results, in the same order as the corresponding input files, plus a
# flag (1=ok, 0=error) indicating whether the match succeeded or not.
sub match_positions ($$$$$) {
    my $ccdpack = shift;	# CCDPack monolith
    my $cat1 = shift;		# First position list
    my $cat2 = shift;		# Second position list
    my $foffopts = shift;	# Reference to hash of findoff options
                                # (may include {error}, {maxdisp},
                                # {complete}, {minsep})
    my $tempfn = shift;		# Temporary filename prefix


    my $cat1out = "$tempfn-cat1.out";
    my $cat2out = "$tempfn-cat2.out";
    if ($noregenerate && -e $cat1out && -e $cat2out) {
	print STDERR "Reusing $cat1out and $cat2out...\n"
	  if $verbose;
	return ($cat1out, $cat2out);
    }

    # Check that the input files exist
    (-r $cat1 && -r $cat2)
      || confess "Can't find FINDOFF input files $cat1 and $cat2\n";

    # Write these two file names to an input file for FINDOFF
    my $findoffinfile = "$tempfn-findoffin";
    open (FOFF, ">$findoffinfile")
      || confess "Can't open file $findoffinfile to write\n";
    print FOFF "$cat1\n$cat2\n";
    close (FOFF);
    push (@tempfiles, $findoffinfile);

    # ... and a file containing the names of the output file
    my $findoffoutfile = "$tempfn-findoffout";
    open (FOFF, ">$findoffoutfile")
      || confess "Can't open file $findoffoutfile to write\n";
    print FOFF "$cat1out\n$cat2out\n";
    close (FOFF);
    push (@tempfiles, $findoffoutfile);

    push (@tempfiles, $cat1out);
    push (@tempfiles, $cat2out);


    # Notes:
    #   ndfnames is false, so restrict and usewcs are ignored.
    #   error=1 is the default.
    #   if minsep is unspecified, set it to be 5*error (this is the 
    #        FINDOFF default, but it appears to be affected by the state of
    #        the ADAM graphics database).
    #   usecomp is false: since I'm comparing only two images, this has no
    #        effect anyway.

    # Construct FINDOFF argument list
    # Input and output filenames
    my $findoffarg = "inlist=^$findoffinfile outlist=^$findoffoutfile";
    # ndfnames is false, so restrict and usewcs are ignored
    $findoffarg .= " ndfnames=false";
    # Log to file
    $findoffarg .= " logto=logfile logfile=$tempfn-findofflog";
    # error=1 is the default, but is a bit parsimonious.  If $foffopts
    # doesn't have an error entry, then pick 5 as a reasonable guess.
    # I'm not sure how reasonable that is, however, especially since a
    # wrongish image scale seems to throw this off badly.  If we have
    # enough points, then a figure more like 20 seems to be more
    # robust.  However, since this depends significantly on the plate
    # scale, this is just guessing.
    my $fofferr = (defined($foffopts->{error}) ? $foffopts->{error} : 20);
    $findoffarg .= " error=$fofferr";
    # It's best to specify minsep explicitly, rather than relying on
    # the dynamic default.
    $findoffarg .= " minsep=".(defined($foffopts->{minsep})
			       ? $foffopts->{minsep}
			       : 5*$fofferr);


    # Other options
    $findoffarg .= " fast=true failsafe=true";
    # Take maxdisp, complete and usecomp (latter two rarely used, I'd
    # think) from $foffopts if present.
    $findoffarg .= " maxdisp=".$foffopts->{maxdisp}
      if defined($foffopts->{maxdisp});
    $findoffarg .= " complete=".$foffopts->{complete}
      if defined($foffopts->{complete});
    $findoffarg .= " usecomp=".$foffopts->{usecomp}
      if defined($foffopts->{usecomp});
    $findoffarg .= ' accept';	# safety net: accept default for any
                                # unspecified parameters

    if ($verbose) {
	print STDERR "Calling findoff\n";
	foreach my $e (split(' ',$findoffarg)) {
	    print STDERR "\t$e\n";
	}
    }

    $ccdpack->contact()
      || confess "Ooops, can't contact the CCDPACK monolith\n";
    my $status = $ccdpack->obeyw ("findoff", $findoffarg);

    # XXX distinguish the case where FINDOFF crashes somehow, from the
    # case where it fails to find matches, and DO NOT croak in the
    # latter case, but merely return some error.

    # If the status returned here is
    # &Starlink::ADAM::DTASK__ACTCOMPLETE, then the match worked, and
    # the two output files should exist.  If the status is something
    # else, then either (a) FINDOFF has crashed or, more likely, (b)
    # it failed to find a match between the position lists.  In case
    # (a) we should croak, but in case (b) we should return normally
    # with the $matchworked flag set to false (in which case the
    # output files probably won't exist).  We can try to distinguish
    # between these two cases by seeing if we can contact the monolith.

    my $matchworked = ($status == &Starlink::ADAM::DTASK__ACTCOMPLETE);
    if (! $matchworked && ! $ccdpack->contact()) {
	confess "Oh dear, it looks like the CCDPACK monolith has just died!\n(did I do that...?)";
    }

    push (@tempfiles, "$tempfn-findofflog");

    return ($cat1out, $cat2out, $matchworked);
}


# Given two FINDOFF output files and a tempfilename, generate an
# ASTROM input file.  
#
# Return a (reference to an) anonymous hash containing keys {filename}
# (the name of the generated ASTROM input file), {nmatches} (the
# number of matches found), {quality} (see below) and {findofferror}
# (a suggested value of the FINDOFF error parameter).
#
# The input files are the FINDOFF output files corresponding to the
# SExtractor and catalogue-query input files.  These have the formats:
#
#    SExtractor:    lineid1 x-pos1  y-pos1  ident1  mag
#    Catalogue:     lineid2 x-pos2  y-pos2  ra      dec  ident2  mag
#
# We want to assemble entries consisting of
#
#    ASTROM:        ra      dec  0.0 0.0 J2000  * ident1/ident2
#                   x-pos1  ypos-1  
#
# for each of the pairs for which lineid1=lineid2.
#
# The return value {quality} is a code indicating the quality of the
# match.  This is an integer ranging from -3 to +3, with
#
#   code   FINDOFF:error 
#    +3     far too high    implausibly good match 
#    +2      a bit high     dubious
#    +1    marginally high
#    -1    marginally low
#    -2      a bit low      ...or the positions flatly don't match
#    -3     far too low     either the positions don't match,
#                           or else FINDOFF:error is way too low
#
# Codes +1 and -1 are `good' matches, but splitting them this way
# allows us to do some CRUDE maximum-likelihood estimation on the
# results of this step.
#
# Single argument is a reference to a hash, containing keys:
#    CCDin          : SExtractor output file -- CCD positions
#    catalogue      : The catalogue of known positions
#    findofferror   : The size of the error parameter used for FINDOFF 
#    helpers        : Helper applications (ref to hash)
#    NDFinfo        : NDF information (ref to hash)
#    tempfn         : Prefix for temporary filenames
# It may additionally contain keys:
#    astrom         : Results from last ASTROM run, or undef or empty hash
#		      if none available (ie, first time) (ref to hash).
#    maxnterms	    : Maximum number of fit terms to try.
#		      6=fit posn, 7=q, 8=centre, 9=q&centre (default 6).
#    findoffboxprop : Proportion of points assumed to lie in the FINDOFF
#		      error box (default 0.5).
#
sub generate_astrom ($) {
    my $par = shift;

    foreach my $k ('CCDin', 'catalogue', 'findofferror', 'helpers',
		   'NDFinfo', 'tempfn') {
	defined($par->{$k})
	  || confess "bad call to generate_astrom: parameter $k not specified\n";
    }

    my $tempfn = $par->{tempfn};

    # Best astrometry so far, as a FITS file, or undef if none yet.
    my $bestastrometry = (defined($par->{astrom})
			  && defined($par->{astrom}->{wcs})
			  ? $par->{astrom}->{wcs}
			  : undef);

    # First, read the two files in.  CCDin first.  The first columns
    # should be identical, being a continuous sequence of integers,
    # starting with 1, so check this on input.
    open (IN, "<$par->{CCDin}")
      || confess "Can't open file $par-{CCDin} to read\n";
    my $lineno = 0;
    my $line;
    my @CCDarray = ();
    while (defined($line = <IN>)) {
	next if ($line =~ /^ *\#/);
	$lineno++;
	my @tmparr = split (' ', $line);
	#$tmparr[0] == $lineno
	#  || confess "File $par->{CCDin} has wrong format ($tmparr[0]!=$lineno)\n";
	push (@CCDarray, \@tmparr);
    }
    close (IN);

    my @CATarray = ();
    open (IN, "<$par->{catalogue}")
      || confess "Can't open file $par->{catalogue} to read\n";
    $lineno = 0;
    while (defined($line = <IN>)) {
	next if ($line =~ /^ *\#/);
	$lineno++;
	my @tmparr = split (' ', $line);
	#$tmparr[0] == $lineno
	#  || confess "File $par->{catalogue} has wrong format\n";
	push (@CATarray, \@tmparr);
    }
    close (IN);

    ($#CCDarray == $#CATarray)
      || confess "generate_astrom: input files have different number of matches\n";


    # {maxnterms} is the maximum number of fit terms to try, and
    # $nterms is the number we will try in fact.  Unless we have more
    # than 10 reference stars _and_ adequate Observation Data, $nterms
    # should be at most 6.  Don't check meteorological data -- the
    # ASTROM documentation suggests that its defaults are reasonable.
    my $nterms = (defined($par->{maxnterms}) ? $par->{maxnterms} : 6);
    if ($nterms > 6 && ! (defined($par->{NDFinfo}->{astromtime})
			  && defined($par->{NDFinfo}->{astromobs})
			  # && defined($par->{NDFinfo}->{astrommet})
			  && defined($par->{NDFinfo}->{astromcol})
			  && ($#CCDarray >= 10))) {
	$nterms = 6;

	if ($#CCDarray < 10) {
	    wmessage ('warning',
		      "Too few matches ($#CCDarray).  Restricted to 6-parameter fit");
	    print STDERR "generate_astrom: Too few matches ($#CCDarray).  Restricted to 6-parameter fit\n" if $verbose;
	} else {
	    my $errmsg = sprintf ("Insufficient obsdata (%s, %s, %s, %s).  Can do only 6-parameter fit",
				  (defined($par->{NDFinfo}->{astromtime})
				   ? "time" : "notime"),
				  (defined($par->{NDFinfo}->{astromobs})
				   ? "obs" : "noobs"),
				  (defined($par->{NDFinfo}->{astrommet})
				   ? "met" : "nomet"),
				  (defined($par->{NDFinfo}->{astromcol})
				   ? "col" : "nocol"));
	    wmessage ('warning', $errmsg);
	    print STDERR "$errmsg\n" if $verbose;
	}
    }

    #+ Want to calculate $\bar x=(\sum^n x_i)/n$ and
    # $M_x=\sum^n(x_i-\bar x)^2 = \sum x_i^2 - (\sum x_i)^2/n$, and
    # the same for $(x\to y)$.  The vector $(\bar x,\bar y)$ is the
    # mean offset of the SExtractor points from the catalogue points,
    # and $M=M_x+M_y$ is the statistic we examine below.
    #
    #-
    my $sumx = 0;
    my $sumy = 0;
    my $sumsq = 0;
    my ($i,$tmp);
    my $nmatches = $#CCDarray + 1;
    for ($i=0; $i<$nmatches; $i++) {
	$tmp += ($CCDarray[$i]->[1] - $CATarray[$i]->[1]);
	$sumx += $tmp;
	$sumsq += $tmp*$tmp;
	$tmp = ($CCDarray[$i]->[2] - $CATarray[$i]->[2]);
	$sumy += $tmp;
	$sumsq += $tmp*$tmp;
    }
    my $xoffset = $sumx/$nmatches;
    my $yoffset = $sumy/$nmatches;
    my $statM = ($sumsq - $sumx*$sumx/$nmatches - $sumy*$sumy/$nmatches);
    #+ |$samplesd| is the sample standard deviation, and is the s.d.\
    # estimated directly from the offsets between the matched CCD and
    # catalogue positions.  It's $|$samplesd|^2=M/(n-1)$.
    #
    #-
    my $samplesd = sqrt($statM/($nmatches-1));
    printf STDERR ("CCD-CAT offset=(%.2f,%.2f)pix, M=%f sample s.d.=%f\n",
		   $xoffset, $yoffset, $statM, $samplesd)
      if $verbose;

#     #+ How good a match is this?
#     #
#     # The quantities $S^2_x=M_x/(n-1)$ and $S^2_y=M_y/(n-1)$ are the sample
#     # variances of the $x$ and $y$ variables.  The variables
#     # $M_x/\sigma^2$ and $M_y/\sigma^2$ each have a $\chi^2$
#     # distribution with $n-1$ degrees of freedom, and hence
#     # $M/\sigma^2$ has a $\chi^2$ distribution with $2(n-1)$ d.o.f.
#     #
#     # %Let us take the mean offset (|$xoffset,$yoffset|) as exact and
#     # %examine the residuals (|$xoffs[]-$xoffset|) and
#     # %(|$yoffs[]-$yoffset|).  We don't really know the distribution of
#     # %the errors in $x$ and $y$, but we can guess it might be normal
#     # %($N(0,\sigma)$).  That means that the variable
#     # %$Z_i=(x_i^2+y_i^2)/\sigma^2$ has a $\chi^2$ distribution with two
#     # %degrees of freedom.  This means in turn that the statistic
#     # %$M=\sum^n Z_i$ also has a $\chi^2$ distribution, but with $2n$
#     # %degrees of freedom.
#     #
#     # We don't know what the $x$ and $y$ standard deviations are,
#     # other than that we suppose them to be equal, but we can estimate
#     # them by supposing that the FINDOFF error box contains some
#     # fraction $p_e$ of the points (take this box to be of side $2e$,
#     # where $e$ is the ERROR parameter for FINDOFF, passed as argument
#     # findofferr).  We have $p_e = Prob(|x|<e \& |y|<e)$, or
#     # $p_e=(P(e/\sigma)-P(-e/\sigma))^2 = [2P(e/\sigma)-1]^2$, where
#     # $P(x)$ is the cumulative probability distribution for the
#     # standard Normal distribution.  Thus $P(e/\sigma)=(\sqrt{p_e}+1)/2$, and
#     # \begin{center}
#     # \catcode`\|=12
#     # \begin{tabular}{r|ccccccc}
#     # $p_e$      & 0.5  & 0.6  & 0.7  & 0.8  & 0.9  & 0.95 & 0.99 \\
#     # \hline
#     # $e/\sigma$ & 1.06 & 1.21 & 1.39 & 1.62 & 1.95 & 2.23 & 2.81 \\
#     # \end{tabular}\end{center}
#     # We can also calculate this on the fly, using subroutine invP:
#     # $e/\sigma=\mbox{\texttt{invP}}(p_e)$
#     #
#     # That is, the analysis below checks the hypothesis that the values
#     # of $p_e$, $e$ and $\sigma$ are consistent.  We choose $p_e$, and 
#     # hence, from the above calculation, implicitly choose
#     # $e/\sigma$, so it's really testing whether the selected
#     # value of $e$ is appropriate.
#     #
#     # In principle we should also worry about the errors in the
#     # offset, but these are down on $\sigma$ by a factor of $n$ or so,
#     # so as long as we have more than about three matches, we can
#     # ignore them.
#     #
#     # Do note, however, that all this is not really worth sweating
#     # about, since the point of this program is that we have unknown
#     # systematic errors in the CCD positions, which it is this
#     # program's task to find out.  However, this is about the only way
#     # we have of dynamically adjusting FINDOFF's `error' parameter, so
#     # it is worth trying to do \emph{something} sensible.
#     #
#     #-
# #    my $statM = 0;
# #    for ($i=0; $i<$nmatches; $i++) {
# #	my $xo = $xoffs[$i]-$xoffset;
# #	my $yo = $yoffs[$i]-$yoffset;
# #	$statM += ($xo*$xo + $yo*$yo);
# #	printf STDERR "  (%9.5f, %9.5f) %f\n", $xo, $yo, $statM;
# #    }
# #    printf STDERR "-- statM=%f\n", $statM;

#     # Debugging/exploration...
#     #    printf "--- %2s %3s%10.3f%10.3f%10.3f%10.3f\n", "e", "dof", 1.06,
#     # 1.95, 2.23, 2.81; printf "--- %2s
#     # %3s%10.3f%10.3f%10.3f%10.3f\n", "-", "---", 0.5, 0.9, 0.95,
#     # 0.99; foreach $serr ((1, 3, 5, 7, 9)) { printf "--- %2d %3d",
#     # $serr, 2*$nmatches; foreach $mult ((1.06, 1.95, 2.23, 2.81)) {
#     # my $sig = $serr/$mult; printf "%10.3f(%5.3f)",
#     # $statM/($sig*$sig), qchi($statM/($sig*$sig), 2*$nmatches); }
#     # print "\n"; } 

#     # Get Q(\chi) for this value of M.
#     my $findoffboxprop = (defined($par->{findoffboxprop})
# 			  ? $par->{findoffboxprop}
# 			  : 0.5); # The proportion of points expected
#                                   # to be in the FINDOFF error box.
#     my $eoversigma = invP($findoffboxprop);
#     my $findofferr = $par->{findofferror};
#     #+ |$findoffsigma| is the value of $\sigma$ that would result in a
#     # fraction |$findoffboxprop| of the points inside the FINDOFF
#     # error box.
#     #-
#     my $findoffsigma = $findofferr/$eoversigma;
#     my $Q = qchi ($statM/($findoffsigma*$findoffsigma), 2*($nmatches-1));
#     my $matchquality;
#     # Test the quality of the match using a two-tailed test at p=0.99
#     # and p=0.95 levels of significance.  That is, compare Q with
#     # 0.5+-(0.99/2) and 0.5+-(0.95/2)
#   MATCHQUALITY: {
# 	if ($Q > 0.995) {
# 	    # An implausibly good match
# 	    printf STDERR ("Very poor (way too good) match\n\t(%d matches, SSD=%f, Q(chi2)=%.5f).\n\tThe FINDOFF error parameter is probably too high.\n\t(findoff errorbox size=%g, prop=%.0f%%, SExtractor sigma=%.2f pixels, Q(samplesd)=%.5f)\n",
# 			   $nmatches, $samplesd, $Q,
# 			   $findofferr, $findoffboxprop*100, $findoffsigma,
# 			   qchi($statM/($samplesd*$samplesd),2*($nmatches-1)))
# 	      if $verbose;
# 	    $matchquality = +3;
# 	    last MATCHQUALITY;
# 	}
# 	if ($Q > 0.975) {
# 	    # A dubiously good match
# 	    printf STDERR ("Rather dodgy (ie, implausibly good) match\n\t(%d matches, SSD=%f, Q(chi2)=%.3f).\n\tThe FINDOFF error parameter may be too high.\n\t(findoff errorbox size=%g, prop=%.0f%%, SExtractor sigma=%.2f pixels)\n",
# 			   $nmatches, $samplesd, $Q,
# 			   $findofferr, $findoffboxprop*100, $findoffsigma)
# 	      if $verbose;
# 	    $matchquality = +2;
# 	    last MATCHQUALITY;
# 	}
# 	if ($Q > 0.025) {
# 	    # A good match, at p=0.95
# 	    printf STDERR ("Good match\n\t(%d matches, SSD=%f, Q(chi2)=%.3f).\n\t(findoff errorbox size=%g, prop=%.0f%%, SExtractor sigma=%.2f pixels)\n",
# 			   $nmatches, $samplesd, $Q,
# 			   $findofferr, $findoffboxprop*100, $findoffsigma)
# 	      if $verbose;
# 	    if ($Q > 0.5) {
# 		$matchquality = +1;
# 	    } else {
# 		$matchquality = -1;
# 	    }
# 	    last MATCHQUALITY;
# 	}
# 	if ($Q > 0.005) {
# 	    # A dubiously poor match
# 	    printf STDERR ("Rather dodgy (ie, implausibly bad) match\n\t(%d matches, SSD=%f, Q(chi2)=%.5f).\n\tEither these position lists do not match,\n\tor else the FINDOFF error parameter is set too low.\n\t(findoff errorbox size=%g, prop=%.0f%%, SExtractor sigma=%.2f pixels)\n",
# 			   $nmatches, $samplesd, $Q,
# 			   $findofferr, $findoffboxprop*100, $findoffsigma)
# 	      if $verbose;
# 	    $matchquality = -2;
# 	    last MATCHQUALITY;
# 	}
# 	# Q<=0.025.  A very poor match
# 	printf STDERR ("Unlikely (ie, implausibly bad) match\n\t(%d matches, SSD=%f, Q(chi2)=%.3f).\n\tEither these position lists simply do not match,\n\tor else the FINDOFF error parameter is far too low.\n\t(findoff errorbox size=%g, prop=%.0f%%, SExtractor sigma=%.2f pixels)\n",
# 		       $nmatches, $samplesd, $Q,
# 		       $findofferr, $findoffboxprop*100, $findoffsigma)
# 	  if $verbose;
# 	$matchquality = -3;
#     }
#     # e/\sigma=$eoversigma, so given that $samplesd is an estimate of
#     # \sigma, estimate the value of e which is consistent with the
#     # $eoversigma we guessed above.
#     my $suggestedfindofferr = $samplesd * $eoversigma;

    # All this statistical thrashing around is pointless, since
    # essentially all of the important errors are systematic ones.  We
    # do as well by simply doubling $samplesd....
    my $suggestedfindofferr = $samplesd * 3.0;

	

    my $astromofile = "$tempfn-astromin";
    open (ASTROMOUT, ">$astromofile")
      || confess "Can't open file $astromofile to write\n";
    push (@tempfiles, "$astromofile");
    my $timestring = localtime();
    print ASTROMOUT "* ASTROM input file generated by...\n* $0\n* $timestring\n";
    print ASTROMOUT "* Star references are SExtractor/Catalogue numbers\n";
    print ASTROMOUT "J2000\n";

    # Write plate distortion record
    my $astrom = $par->{astrom};
    my $plateq = (defined($astrom) && defined($astrom->{q}))
		  ? $astrom->{q} 
		  : 0.0;
    if ($nterms == 7 || $nterms == 9) {
	# Fit plate distortion
	print ASTROMOUT "~ ";
    }
    print ASTROMOUT "GENE $plateq\n";	# General distortion model

    # Determine the SKY coordinates of the projection pole (typically
    # the geometrical centre of the plate).  If $astrom is defined,
    # then we have already been through ASTROM at least once, and we
    # (should) have an estimate of the pole position (possibly, but
    # not necessarily, improved from the user's original estimate).
    # Use this value if it's available.
    my @projpolesex;		# Plate centre, as sexagesimal coordinates
    if (defined($astrom)
	&& defined($astrom->{rasex})
	&& defined($astrom->{decsex})) {
	($projpolesex[0] = $astrom->{rasex})  =~ s/:/ /g;
        ($projpolesex[1] = $astrom->{decsex}) =~ s/:/ /g;
	# Easy!
    } else {
	# Determine the plate centre.  If the argument $astrom
	# is defined, then $astrom->{wcs} holds a FITS file
	# with the best astrometry so far.  If not, then the best we
	# can do is use the first-guess astrometry which was initially
	# passed to moggy.
	#
	# Do the calculation by getting the plate centre
	# in pixels, and converting this to SKY
	# coordinates.  par->{NDFinfo}->{[xy][01]} points to coordinates
	# (lower-x, lower-y, upper-x, upper-y) for the NDF, and we take
	# the plate centre as the average of these.  
	my @projpole;	# Centre of plate, sky coordinates, dec.deg.
	# XXX replace x0 etc with dim1, dim2
	# my $pcx = ($par->{NDFinfo}->{x0} + $par->{NDFinfo}->{x1})/2.0;
	# my $pcy = ($par->{NDFinfo}->{y0} + $par->{NDFinfo}->{y1})/2.0;
	# These are in the GRID domain (centre of first pixel at (1,1)),
	# so to get the plate centre, we need to add one to each dimension.
	my $pcx = ($par->{NDFinfo}->{dim1} + 1)/2.0;
	my $pcy = ($par->{NDFinfo}->{dim2} + 1)/2.0;
	if (defined($astrom) && defined($astrom->{wcs})) {
	    # Do the calculation by getting the plate centre in pixels,
	    # and converting this to SKY coordinates using ATOOLS/asttrann
	    my @row = ($pcx, $pcy);
	    my @dat = (\@row);
	    my $inndfname = twodarray2ndf (@dat, $par->{tempfn}.'-coordtrans');
	    my $outndfname = "$inndfname-out";
	    my $asttranarg = "this=$astrom->{wcs}";
	    $asttranarg .= " in=$inndfname out=$outndfname forward=true";
	    print STDERR "Calling asttrann $asttranarg...\n"
	      if $verbose;
	    my $status = $par->{helpers}->{atools}->obeyw("asttrann",
							  $asttranarg);
	    ($status == &Starlink::ADAM::DTASK__ACTCOMPLETE)
	      || carp "generate_astrom: error running asttrann";
	    push (@tempfiles, ("$outndfname.sdf", "$inndfname.sdf"));
	    my $skyd = ndf2twodarray ($outndfname);
	    @projpole = @{$skyd->[0]};
	    $projpole[0] *= $d2r;
	    $projpole[1] *= $d2r;
	    print STDERR "Plate centre: ($pcx,$pcy)->($bestastrometry)->($projpole[0],$projpole[1])\n"
	      if $verbose;
	} else {
	    # Apply the offset obtained from the last step.  The offset is
	    # calculated as (CCD-catalogue), that is, an estimate of
	    # the amount that the CCD pixel positions are offset from
	    # the pixel positions they should have, given the rough original
	    # astrometry information.  If we subtract this offset from the
	    # plate centre, then that is the `correct' location of the plate
	    # centre (given this astrometry), and if we convert this pixel
	    # position back to Sky coordinates, we have an estimate of the Sky
	    # coordinates of the plate centre, which we can give to Astrom.
	    $pcx -= $xoffset;
	    $pcy -= $yoffset;
	    my $projpoleref = $par->{helpers}->{moggy}->astconvert($pcx, $pcy, 1);
	    defined($projpoleref)
	      || confess "generate_astrom: Can't convert coordinates";
	    @projpole = @$projpoleref;
	    print STDERR "Plate centre: offset ($xoffset,$yoffset): ($pcx,$pcy)->($projpole[0],$projpole[1])\n"
	      if $verbose;
	}
	$projpolesex[0] = deg2sex ($projpole[0], 1);
        $projpolesex[1] = deg2sex ($projpole[1], 0);
    }

    # Write the ASTROM plate centre line
    if ($nterms == 8 || $nterms == 9) {
	# Fit plate centre
	print ASTROMOUT "~ ";
    }
    printf ASTROMOUT ("%s   %s   J2000  %s  * Plate centre\n",
		      $projpolesex[0], $projpolesex[1],
		      $par->{NDFinfo}->{date});

    # Write observation data records
    printf ASTROMOUT ("Time %s\t* %s\n",
		      $par->{NDFinfo}->{astromtime},
		      $par->{NDFinfo}->{astromtimecomment})
      if (defined($par->{NDFinfo}->{astromtime}));
    printf ASTROMOUT ("Obs %s\t* %s\n",
		      $par->{NDFinfo}->{astromobs},
		      $par->{NDFinfo}->{astromobscomment})
      if (defined($par->{NDFinfo}->{astromobs}));
    printf ASTROMOUT ("Met %s\t* %s\n",
		      $par->{NDFinfo}->{astrommet},
		      $par->{NDFinfo}->{astrommetcomment})
      if (defined($par->{NDFinfo}->{astrommet}));
    printf ASTROMOUT ("Col %s\t* %s\n",
		      $par->{NDFinfo}->{astromcol},
		      $par->{NDFinfo}->{astromcolcomment})
      if (defined($par->{NDFinfo}->{astromcol}));

    for ($i=0; $i<=$#CATarray; $i++) {
	printf ASTROMOUT "%s   %s  0.0  0.0  J2000\t* %d/%d\n",
	    deg2sex($CATarray[$i]->[3], 1),
	    deg2sex($CATarray[$i]->[4], 0),
	    $CCDarray[$i]->[4], $CATarray[$i]->[5];
	printf ASTROMOUT "%12f   %12f\n", $CCDarray[$i]->[1],
	    $CCDarray[$i]->[2];
    }

    print ASTROMOUT "END\n";
    close (ASTROMOUT);

    return {filename => $astromofile,
	    nmatches => $nmatches,
	    findofferror => $suggestedfindofferr};
}

# Run astrom, using the given input file.  
#
# Return an array of references to anonymous hashes, each containing
# fields {nterms}, {q}, {rarad}, {decrad} (RA and Dec in radians),
# {rasex}, {decsex} (RA and Dec in sexagesimal notation), {wcs} (name
# of FITS-WCS file), {nstars}, {prms} (RMS residual in pixels),
# {STATUS} (true if the fit was OK, false otherwise).
#
# Return information about _all_ the fits, even the ones which didn't
# work.  The field {STATUS} will always be defined, and if the fit
# didn't work, it'll be set to 0=false.
#
# The residual is the sum of the squares of the X and Y pixel residuals.
#
# The nterms field is the number of terms in the astrom fit.
# It is 6, 7, 8 or 9, with 6=normal fit, 7=fit q, 8=fit plate centre,
# 9=fit q and plate centre.
#
# Return undef on error.
sub run_astrom ($$) {
    my ($astromin, $tempfn) = @_;

    my $arep = "$tempfn-astrom.report";
    my $asum = "$tempfn-astrom.summary";
    my $alog = "$tempfn-astrom.log";
    printf STDERR ("Starting %s\t\\\n\t%s\t\\\n\t%s\t\\\n\t%s\t\\\n\t%s\t\\\n\t%s\t\\\n\t%s\n",
		   "$ENV{ASTROM_DIR}/astrom.x",
		   '.',
		   $astromin,
		   $arep,
		   $asum,
		   "$tempfn-astromout-wcs",
		   $alog)
      if $verbose;
    my $astromexit = system ("$ENV{ASTROM_DIR}/astrom.x",
			     '.',
			     $astromin,
			     $arep,
			     $asum,
			     "$tempfn-astromout-wcs",
			     $alog);

    push (@tempfiles, ($arep, $asum, $alog));

    # Read in the log, and search for the names of the FITS WCS
    # files.  These are in lines of the form 'FITS filename'
    my @ret;
    open (SUM, "<$alog") || do {
	print STDERR "Can't open ASTROM log file $alog\n";
	return undef;
    };
    my $l;
    my %results;
    while (defined ($l=<SUM>)) {
	chop $l;
	($l =~ /^FIT/) && do {
	    %results = ();
	    $results{STATUS} = 0; # initialise to false
	    next;
	};

	($l =~ /^RESULT +(\S+)\s+(\S+)/)
	  && do { $results{$1} = $2;
		  next; };

	($l =~ /^STATUS +(\S+)/)
	  && do {
	      if ($1 eq 'OK') {
		  $results{STATUS} = 1;
	      } elsif ($1 eq 'BAD') {
		  $results{STATUS} = 0;
	      } else {
		  print STDERR "ASTROM logfile: unrecognised STATUS\n";
		  $results{STATUS} = 0;
	      }
	      next;
	  };

	($l =~ /^ENDFIT/) && do {
	    scalar(%results) || do {
		print STDERR "ASTROM logfile: no results!\n";
		next;
	    };
	    my %t = %results;	# create new hash
	    push (@ret, \%t);
	    next;
	};
    }
    close (SUM);

    @ret || print STDERR "run_astrom: no return values! Either astrom failed, or the log file was incomplete\n";

    return @ret;
}



# Convert decimal degrees to sexagesimal.
#
# Args:
#     $val  = angle in decimal degrees
#     $isra = if true (1), angle is a RA (return HMS), if false (0), 
#             it's a Dec (return DMS)
#     $sep  = if present, use this as separator (default ' ')
#
# Return undef on range errors.
sub deg2sex ($$;$) {
    my ($val, $isra, $sep) = @_;
    my ($dh, $min, $sec, $mas, $sign);
    # Check we haven't been given a sexagesimal value by mistake.
    if ($val !~ /^ *[-+]?[0-9]*(\.[0-9]*)? *$/) {
	print STDERR "deg2sex: decimal angle $val malformed\n";
	return undef;
    }
    defined($sep) || ($sep = ' ');
    # Add 0.5mas to the value, and do rounding explicitly.  Otherwise
    # values such as 40 degrees turns into 2.66..667 hours,
    # 2:39:59.999..., which is rounded to 2:39:60 rather than 2:40:00
    if ($isra) {
	$val += 0.0000001388;
	while ($val < 0) { $val += 360; }
	while ($val >= 360) { $val -= 360; }
	$val /= 15;
	$sign = 1;
    } else {
	#$sign = ($val == 0 ? 1 : $val/abs($val));
	#$val = abs($val);
	if ($val >= 0) {
	    $sign = 1;
	} else {
	    $sign = -1;
	    $val = -$val;
	}
	if ($val > 90) {
	    print STDERR "deg2sex: decimal Dec $val out of range\n";
	    return undef;
	}
	# val=90.0000001388 will round to 90
	$val += 0.0000001388;
    }
    $dh = int($val);
    $val = ($val - $dh) * 60;
    $dh *= $sign;
    $min = int($val);
    $val = ($val - $min) * 60;
    $sec = int($val);
    $mas = int(($val - $sec) * 1000);
    return sprintf ("%d%s%02d%s%02d.%03d", $dh, $sep, $min, $sep, $sec, $mas);
}

# Given a string with a (colon-separated) sexagesimal angle, turn it
# into decimal degrees.
#
# If the second argument is true, the first argument is RA in HMS; if
# false, it's Dec in DMS.  If angle argument is undef, silently return
# undef.
#
# Return undef on formatting/range errors.
sub sex2deg ($$) {
    my ($val, $isra) = @_;

    defined($val) || return undef;

    my ($dh,$min,$sec);
    if (($dh,$min,$sec) = ($val =~ /^ *([-+]?[0-9]+):([0-9]+):([0-9]+(\.[0-9]+)?) *$/)) {
	my $rval = $dh + $min/60.0 + $sec/3600.0;
	if ($isra) {
	    $rval *= 15;	# Hours --> degrees
	    if ($rval == 360.0) { $rval = 0.0; } # accept 360, but fix
	    if ($rval < 0.0 || $rval >= 360.0) {
		print STDERR "sex2deg: sexagesimal RA $val --> $rval out of range\n";
		return undef;
	    }
	} else {
	    if ($rval < -90.0 || $rval > +90.0) {
		print STDERR "sex2deg: sexagesimal Dec $val --> $rval out of range\n";
		return undef;
	    }
	}
	# All OK
	return $rval;
    } else {
	print STDERR "sex2deg: sexagesimal angle $val malformed\n";
	return undef;
    }
}


# Calculate $Q(\chi^2,\nu) = \int_{\chi^2}^\infty\dots$, from A&S 26.4.5.
sub qchi ($$) {
    my ($chi2, $dof) = @_;
    my $nu22 = ($dof-2.0)/2.0;
    my $summand = 1;
    my $fracsum = 0;
    my $r;
    for ($r=1; $r<=$nu22; $r++) {
	$summand *= $chi2/(2*$r);
	$fracsum += $summand;
    }
    return (exp(-$chi2/2.0) * (1 + $fracsum));
}

# There are certain restrictions on what characters can be used in NDF
# names (specifically, `.' is forbidden).  Given a potential NDF file
# name (ie, including path), canonicalise and return it.  Return undef
# on error.
sub canonicalise_ndfname ($) {
    my $ndfname = shift;

    my ($path,$name) = ($ndfname =~ m{(.*/)?(.*)});
    $name =~ s/[^a-zA-Z0-9_-]/_/g;
    if ($path eq '') {		# no path
	return $name;
    } else {
	return "$path$name";
    }
}

# Write an array to an NDF file.  The arguments are an array[n] of
# references to arrays, which we'll insist are all the same length, m,
# and the name of an NDF.  The output array in the NDF has dimensions
# (m,n).  Note that the dimensions are swapped, turning what is
# efficient for Perl into what is efficient for (fortran-style) NDF
# arrays. 
#
# Note that there are restrictions on what characters can be used in
# NDF names (specifically `.' is forbidden).  The routine will
# silently change any forbidden characters to _, so you should always
# use the NDF name as returned by this routine, rather than assuming
# that the name used as input is acceptable.
#
# This and the following routine, are largely lifted from ndf.t, one of
# the test files for the Starlink::NDF module.
#
# Return the name of the NDF on success, undef on error;
sub twodarray2ndf (\@$) {
    my ($marray, $NDFname) = @_;

    my @dat = @$marray;

    # Canonicalise the requested name
    $NDFname = canonicalise_ndfname($NDFname);

    my $i;
    my $subdim = 0;
    for ($i=0; $i<=$#dat; $i++) {
	my @t = @{$dat[$i]};
	if ($i == 0) {		# first time round
	    $subdim = $#t;
	} else {
	    if ($subdim != $#t) {
		$subdim = -$i;	# flag value
		last;
	    }
	}
    }
    if ($subdim <= 0) {
	print STDERR ("twodarray2ndf: inconsistent array dimensions: row %d has %d entries, not %d\n",
		      $i, $#{$dat[$i]}, $#{$dat[0]})
	  if $verbose;
	return 0;
    }

    my @ubnd = ($subdim+1, $#dat+1);
    my @lbnd = (1,1);

    # Make an array to be written.
    my @alldata = ();
    foreach $i (0..$#dat) {
	push (@alldata, @{$dat[$i]});
    }

    # Initialise status.
    my $ndfstatus = &NDF::SAI__OK;

    # Initialise the NDF system
    ndf_begin();

    # Create a new container file, and map the array
    my ($place, $indf, $pntr, $el);
    ndf_place (NDF::DAT__ROOT(), $NDFname, $place, $ndfstatus);
    ndf_new ('_DOUBLE', 2, @lbnd, @ubnd, $place, $indf, $ndfstatus);
    ndf_map ($indf, 'DATA', '_DOUBLE', 'WRITE', $pntr, $el, $ndfstatus);

    array2mem (@alldata, "d*", $pntr) if ($ndfstatus == &NDF::SAI__OK);

    # Clean up and close the file
    ndf_unmap ($indf, 'DATA', $ndfstatus);
    ndf_annul ($indf, $ndfstatus);
    ndf_end ($ndfstatus);

    ($ndfstatus == &NDF::SAI__OK) || ($NDFname = undef);

    return $NDFname;
}

# Read the array contents of an NDF into a 2-d array.  The argument is
# the name of an NDF, and the return value a reference to an array of
# references to arrays.
#
# The array in the input NDF must be a 2-d array, (m,n), and if it is
# not, the routine returns undef.
#
# If the input array has the correct dimensions, the routine returns a
# reference to an array[n] of references to arrays[m] of floats.  As
# before, note that the array dimensions are swapped.
sub ndf2twodarray ($) {
    my $NDFname = shift;

    my ($ndfstatus, $indf, $maxdims, @dim, $ndim, $pntr, $el);
    my @data;

    # Open the file
    $ndfstatus = &NDF::SAI__OK;
    ndf_begin();
    ndf_find (NDF::DAT__ROOT(), $NDFname, $indf, $ndfstatus);

    # Check the dimensions.  If there are more than two dimensions,
    # $ndfstatus will be returned as NDF__XSDIM, and this will be
    # reported as an error at the end of the routine.
    $maxdims = 2;
    ndf_dim ($indf, $maxdims, @dim, $ndim, $ndfstatus);

    # Map the data array, and unpack it into the array @data
    ndf_map ($indf, 'DATA', '_DOUBLE', 'READ', $pntr, $el, $ndfstatus);
    @data = mem2array ($pntr, "d*", $el) if ($ndfstatus == &NDF::SAI__OK);

    # Close and clean up
    ndf_unmap ($indf, 'DATA', $ndfstatus);
    ndf_annul ($indf, $ndfstatus);
    ndf_end ($ndfstatus);

    if ($ndfstatus != &NDF::SAI__OK) {
	# Didn't work, for some reason
	return undef;
    }

    # Construct the returned array
    my @retarr;
    my $offset = ($dim[1]-1)*$dim[0];
    while ($offset >= 0) {
	my @t = splice (@data, $offset);
	unshift (@retarr, \@t);
	$offset -= $dim[0];
    }

    return \@retarr;
}

# Given the name of a text file, read it in, ignoring # comments, and
# create and return an array of references to lines of the file.
# Return an empty array on error.
sub txt2arr ($) {
    my $txtfn = shift;
    my @d;
    my $line;
    open (TIN, "<$txtfn") || return ();
    while (defined($line = <TIN>)) {
	chop ($line);
	$line =~ s/ *\#.*$//;	# strip comments
	next if ($line =~ /^[ \t]*$/); # ignore (newly-)empty lines
	my @t = split (' ', $line);
	push (@d, \@t);
    }
    close (TIN);

    return @d;
}

# Given a file containing a grid of numbers, turn its contents into
# an NDF, returning the name of the NDF file.
#
# Note that there are restrictions on what characters can be used in
# NDF names (specifically `.' is forbidden).  The routine will
# silently change any forbidden characters to _, so you should always
# use the NDF name as returned by this routine, rather than assuming
# that the name used as input is acceptable.
#
# Return the NDF name, or undef on error.
sub txt2ndf ($;$) {
    my ($txtfn,$ndfname) = @_;

    my @d = txt2arr ($txtfn);
    if (! defined ($ndfname)) {
	$ndfname = $txtfn;
	# Remove any `standard' extension
	$ndfname =~ s/\.[a-zA-Z]*$//;
    }

    # Canonicalise the requested name
    $ndfname = canonicalise_ndfname($ndfname);

    my $rval = undef;
    if ($#d >= 0) {		# no error
	$rval = twodarray2ndf (@d, $ndfname);
	# Returns undef on error
    }

    return $rval;
}

# Dump an NDF file into a text file.  Return text file name, or undef on error.
sub ndf2txt ($;$) {
    my ($ndfname, $txtname) = @_;

    my $d = ndf2twodarray ($ndfname);
    defined ($d)
      || do {
	  print STDERR "No NDF $ndfname found\n";
	  return undef;
      };

    defined ($txtname) || ($txtname = "$ndfname.txt");
    open (AOUT, ">$txtname")
      || do {
	  print STDERR "Can't open file $txtname to write\n";
	  return undef;
      };
    my $row;
    foreach $row (@$d) {
	print AOUT join ("\t", @$row), "\n";
    }
    close (AOUT);

    return $txtname;
}

sub reuse_files (;$) {
    my $arg = shift;
    if (defined($arg)) {
	$noregenerate = !$arg;
    } else {
	$noregenerate = 1;
    }
}

sub get_temp_files () {
    return @tempfiles;
}

# Convert year,month,day (noon) to Julian Day number.
#
# This formula is from Graham Woan's `The Cambridge Handbook of Physics
# Formulas'.
sub ymd2jd ($$$) {
    # Use the standard formula to convert Gregorian dates to Julian
    # Day numbers
    use integer;
    my ($year, $month, $day) = @_;
    # Year is (1000..3000), month is in (1..12), day in (1..31).  The
    # restriction on year is not because of any limitation on the
    # validity of the formula, but to guard against silly parameters
    # (eg, 2-digit dates).
    ($year > 1000 && $year < 3000)|| confess "ymd2jd: years in 1000..3000 only";
    ($month >= 1 && $month <= 12) || confess "ymd2jd: month $month out of range";
    ($day >= 1 && $day <= 31)     || confess "ymd2jd: day $day out of range";

    return $day - 32075 + 1461*($year+4800+($month-14)/12)/4
      +367*($month-2-($month-14)/12*12)/12
      -3*(($year+4900+($month-14)/12)/100)/4;
}

# Convert Julian day to Julian epoch (decimal year)
#
# The conversion from JD is from Robin Green's `Spherical Astronomy',
# section 10.5
sub jd2je ($) {
    my $jd = shift;
    return 2000.0 + ($jd - 2451545)/365.25;
}

# Convert year,month,day (noon) to a Julian epoch (decimal year)
# Assumes input parameters are within valid ranges.
sub ymd2je ($$$) {
    my ($year,$month,$day) = @_;

    return jd2je (ymd2jd($year, $month, $day));
    #return 2000 + (ymd2jd($year, $month, $day) - 2451545)/365.25;
}

# Convert a FITS date into a Julian epoch.  After the 1997-11-10
# amendment documented in
# <ftp://nssdc.gsfc.nasa.gov/pub/fits/year2000_agreement.txt>, FITS
# dates can be in one of the forms YYYY-MM-DDThh:mm:ss[.s...],
# YYYY-MM-DD or DD/MM/YY, with the last representing _only_ dates
# between 1900-1999.
#
# If no time is given, the time is taken to be noon (by ymd2jd).
#
# Return a Julian epoch, or undef if the date is malformed.  Allow
# leading and trailing space, but otherwise be strict.
sub parse_fits_date ($) {
    my $fdate = shift;
    if ($fdate =~ /^ *(\d{4})-(\d{2})-(\d{2})(T(\d{2}):(\d{2}):(\d{2}(\.\d+)?))? *$/) {
	if (defined($4)) {
	    my $jd = ymd2jd ($1,$2,$3);
	    # Add time: ymd2jd returns JD at noon.  Cf ymd2je
	    return 2000 + ($jd
			   + (($5-12)*3600 + $6*60 + $7)/86400.0
			   - 2451545)/365.25;
	} else {
	    return ymd2je ($1,$2,$3);
	}
    } elsif ($fdate =~ m{^ *(\d{2})/(\d{2})/(\d{2}) *$}) {
	return ymd2je ($3+1900, $2, $1);
    } else {
	return undef;
    }
}

#+
# Solve the equation $P(\rho) = (\sqrt{p_e)+1)/2$, for $\rho$.
#
# Use Newton-Raphson, and the approximation
# $P(\rho)=1-Z(\rho)(a_1t+a_2t^2+a_3t^3)$, with $t=1/(1+p_t\rho)$
# (A\&S 26.2.16).  That results in $\rho_{i+1} = \rho_i +
# ((a_3t+a_2)t+a_1)t + (\sqrt\p_e-1)/2Z(\rho)$.
#-
sub invP ($) {
    my $pe = shift;

    my $spm1 = (sqrt($pe)-1.0)/2.0;
    my $pcoef = 0.33267;	# coefficients: see (A&S 26.2.16)
    my $a1 = 0.4361836;
    my $a2 = -0.1201676;
    my $a3 = 0.9372980;
    my $stwopi = 2.50662827463;	# \sqrt{2\pi}
    my $accuracy = 0.0001;	# Accuracy of the result (note (A&S
                                # 26.2.16) is accurate to only 1e-5).
    my $itercount = 10;		# Stop the process running away.

    my $rhomod;			# Increment, tested against $accuracy.
    my $rho = 1;		# Initial value.


    do {
	my $t = 1.0/(1.0+$pcoef*$rho);
	my $Z = exp(-$rho*$rho/2.0)/$stwopi;

	$rhomod = (($a3*$t + $a2)*$t + $a1)*$t + $spm1/$Z;
	$rho += $rhomod;
    } while ($rhomod > $accuracy && --$itercount>0);

    return $rho;
}

# Create a FITS-WCS header from a keyword-value hash, plus the NDFinfo
# hash.
#
# Keywords are ra (RA of the centre of the pixel grid, in
# colon-separated hms or decimal degrees), dec (in dms or degrees), pa
# (rotation angle, see below: default 0), scale (arcsec/pixel: default
# 1), invert (see below: default 0).  If any other keywords are found,
# return with an error.
#
# The position angle is the rotation, in degrees clockwise, of the
# pixel grid relative to the RA-Dec axes, and thus the rotation, in
# degrees anticlockwise, which the pixel grid has to be rotated.  The
# resulting (x_1,x_2)=(RA,Dec) plane has RA=x_1 increasing to the
# right.  If $invert is true, additionally flip the x_1 axis.
#
# See C&G (Calabretta and Greisen, `Representation of celestial
# coordinates in FITS'; draft at
# <http://www.cv.nrao.edu/fits/documents/wcs/wcs.html>, due to appear
# in A&A eventually).
#
# Return a reference to an array holding a sequence of FITS cards, or
# undef on error.  The routine should produce a valid sequence of FITS
# cards, so the SIMPLE, BITPIX and NAXIS keywords are included.
sub make_pseudo_fits (\%\%) {
    my ($kv,$NDFinfo) = @_;

    (defined($kv->{ra}) && defined($kv->{dec})) || do {
	print STDERR "--wcs option must have at least ra and dec\n";
	return undef;
    };

    my $radeg  = ($kv->{ra}=~/:/  ? sex2deg($kv->{ra},1)  : $kv->{ra});
    my $decdeg = ($kv->{dec}=~/:/ ? sex2deg($kv->{dec},0) : $kv->{dec});

    # Did the sex2deg conversion work?
    defined($radeg) || do {
	print STDERR "make_pseudo_fits: can't convert RA $kv->{ra}\n";
	return undef;
    };
    defined($decdeg) || do {
	print STDERR "make_pseudo_fits: can't convert Dec $kv->{dec}\n";
	return undef;
    };
    # Range checks
    ($radeg >= 0.0 && $radeg < 360.0) || do {
	print STDERR "make_pseudo_fits: RA $radeg out of range\n";
	return undef;
    };
    ($decdeg >= -90.0 && $decdeg <= +90.0) || do {
	print STDERR "make_pseudo_fits: Dec $decdeg out of range\n";
	return undef;
    };

    my @pfitsarray = ();
    push (@pfitsarray, sprintf ("%-8s= %20s", 'SIMPLE', 'T'));
    push (@pfitsarray, sprintf ("%-8s= %20d", 'BITPIX', 8));
    push (@pfitsarray, sprintf ("%-8s= %20d", 'NAXIS', 0));
    # Coordinate types: gnomonic - zenithal with \mu=0
    push (@pfitsarray, sprintf ("%-8s= '%-10s'", 'CTYPE1', 'RA---AZP'));
    push (@pfitsarray, sprintf ("%-8s= '%-10s'", 'CTYPE2', 'DEC--AZP'));
    push (@pfitsarray, sprintf ("%-8s= %20f", 'PV2_1',  0)); # \mu
    # LONPOLE -- following is default (see C&G)
    push (@pfitsarray, sprintf ("%-8s= %20f", 'LONPOLE',($decdeg==90?0:180)));
    my $logstring='';
    foreach my $testkw (keys(%$kv)) {
	$logstring .= ' '.$testkw.'='.$kv->{$testkw};
    }
    print STDERR "logstring=$logstring\n";
    push (@pfitsarray, sprintf ("%-8s %.71s", 'COMMENT', $logstring));
    # Coordinate value at reference point
    push (@pfitsarray, sprintf ("%-8s= %20f / RA    %s",
				'CRVAL1', $radeg,deg2sex($radeg,1)));
    push (@pfitsarray, sprintf ("%-8s= %20f / Dec   %s",
				'CRVAL2', $decdeg, deg2sex($decdeg,0)));
    # units (default degrees)
    push (@pfitsarray, sprintf ("%-8s= '%-10s'", 'CUNIT1', 'deg'));
    push (@pfitsarray, sprintf ("%-8s= '%-10s'", 'CUNIT2', 'deg'));
    # Pixel location of reference point
    push (@pfitsarray, sprintf ("%-8s= %20f", 'CRPIX1',
				($NDFinfo->{dim1}+1)/2));
    push (@pfitsarray, sprintf ("%-8s= %20f", 'CRPIX2',
				($NDFinfo->{dim2}+1)/2));

    my $pa = (defined($kv->{pa}) ? $kv->{pa} : 0);
    my $scale = (defined($kv->{scale}) ? $kv->{scale} : 1);
    my $invert = (defined($kv->{invert}) && $kv->{invert} != 0);

    # Write the CD matrix, which rotates and scales the pixel coordinates
    # to projection-plane coordinates (aka intermediate world
    # coordinates).
    my $acospa = $scale/3600.0*cos($pa/$d2r);
    my $asinpa = $scale/3600.0*sin($pa/$d2r);
    if ($invert) {
	push (@pfitsarray, sprintf ("%-8s= %20G", 'CD1_1', -$acospa));
	push (@pfitsarray, sprintf ("%-8s= %20G", 'CD1_2', -$asinpa));
    } else {
	push (@pfitsarray, sprintf ("%-8s= %20G", 'CD1_1', $acospa));
	push (@pfitsarray, sprintf ("%-8s= %20G", 'CD1_2', $asinpa));
    }
    push (@pfitsarray, sprintf ("%-8s= %20G", 'CD2_1', -$asinpa));
    push (@pfitsarray, sprintf ("%-8s= %20G", 'CD2_2', $acospa));

    # Add coordinate system keywords (FK5, J2000.0).  With (presumed)
    # rough astrometry, it's unlikely to make much difference, but it
    # resolves ambiguities and warnings when the results are read elsewhere.
    push (@pfitsarray, sprintf ("%-8s= '%s'", 'RADESYS', 'FK5'));
    push (@pfitsarray, sprintf ("%-8s= '%s'", 'EQUINOX', '2000.0'));

    # End card
    push (@pfitsarray, 'END     ');

    return \@pfitsarray;
}

# Check a (reference to a) hash against another hash.  The keywords in
# the first hash must all be present in the second, and the values of
# the keyword in the first must match the regexps which are the values
# of the keywords in the second.
#
# Return undef on _success_, and the malformed keyword if any keyword
# doesn't match.
sub check_kwd_list ($$) {
    my ($t,$m) = @_;

    foreach my $k (keys(%$t)) {
	return $t->{$k} unless defined($m->{$k});
	my $pattern = '^'.$m->{$k}.'$';
	return $t->{$k} unless $t->{$k} =~ m{$pattern};
    }
    return undef;
}

# Wmessage writes a message on the stdout.  $1 is one of `info',
# `warning', `fatal', with unrecognised strings being fatal (checks
# for errors).   $2 is the message.  In the case of `fatal'
# messages, the routine then croaks.
sub wmessage ($$) {
    my $type = uc(shift());
    my $msg = shift;
    my @prefixes = ('-- ', '!! ', 'XX ');
    my $prefix = undef;
    my $croak = 0;
    

    if ($type eq 'INFO') {
	$prefix = $prefixes[0];
    } elsif ($type eq 'WARNING') {
	$prefix = $prefixes[1];
    } elsif ($type eq 'FATAL') {
	$prefix = $prefixes[2];
	$croak = 1;
    } else {
	$prefix = "$prefixes[2] XXXX WHAT TYPE OF MESSAGE?    ";
	$croak = 1;
    }

    print "$prefix$msg\n";
    if ($croak) {
	# This message goes to stderr, and we die
	croak "$prefix$msg\n";
    }

    return;			# we won't get here if $croak is true
}

sub verbosity ($) {
    $verbose = shift;
}

1;
