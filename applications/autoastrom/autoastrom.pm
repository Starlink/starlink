#
# Subroutines for autoastrom.pl
#
# Broken into a separate file (a) in case I want to reuse them, and
# (b) so that I can run regression tests on them.
#
# $Id$

use strict;
use Carp;

# Declare subroutines with prototypes
sub extract_objects ($$$$);
sub ndf_info ($$$);
sub get_catalogue ($\%$$);
sub match_positions ($$$$$);
sub generate_astrom ($);
sub run_astrom ($$);
sub dmshms ($$);
sub ymd2dec ($$$);
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
sub invP ($);

my $noregenerate = 0;
my @tempfiles = ();

# Useful values
my $d2r = 57.295779513082320876798155; # degrees to radians (quite accurately)

sub extract_objects ($$$$) {
    my ($helpers, $ndfname, $maxobj, $tempdir) = @_;
    # Extract objects from the NDF file $ndfname, returning the
    # filename of the resulting catalogue, using $tempdir as a path for
    # temporary files, and returning a maximum of $maxobj objects.
    # The file which is returned should be suitable for input to
    # FINDOFF.  On error, return undef.


    my $extractor = $helpers->{extractor};
    my $catname = "$tempdir/extractor";
    if ($noregenerate && -e $catname) {
	# Nothing to do
	print STDERR "Reusing $catname...\n";
	return $catname;
    }

    push (@tempfiles, $catname);

    # We need to tell SExtractor where to put the catalogue output.
    # The custom configuration file we use sets CATALOG_NAME to have
    # the value $AUTOASTROMTEMPCATALOGUE.  This should have been set
    # up already, but check it here just in case.
    unless (defined $ENV{AUTOASTROMTEMPCATALOGUE}) {
	$ENV{AUTOASTROMTEMPCATALOGUE} = "$tempdir/extractor.temp";
	push (@tempfiles, $ENV{AUTOASTROMTEMPCATALOGUE});
    }
    my $tcat = $ENV{AUTOASTROMTEMPCATALOGUE};

#    my $extractor = new Starlink::AMS::Task
#      ("extractor_mon_$$", "$ENV{EXTRACTOR_DIR}/extractor");
#    $extractor->contactw  || die "Error launching extractor -- timeout";

    my $parlist = "image=$ndfname config=$ENV{AUTOASTROM_DIR}/extractor.config keywords=false";
    print STDERR "parlist=$parlist\n";
    my $status = $extractor->obeyw ("extractor", $parlist);

    $status == &Starlink::ADAM::DTASK__ACTCOMPLETE
      || die "Error running extractor";

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
    while (defined($line = <CAT>)) {
	chomp $line;
	next if ($line !~ /^ *[0-9]/);
	my @l = split (' ', $line);
	$cathash{$l[7]} = sprintf ("%5d %12.3f %12.3f %5d %12.1f",
				   $l[0], $l[1], $l[2], $l[0], $l[7]);
    }
    close (CAT);

    my @sortcat = sort {$b <=> $a} keys(%cathash);

    open (CAT, ">$catname") || return undef;
    print CAT "# SExtractor catalogue, munged by $0\n";
    while ($maxobj > 0 && $#sortcat >= 0) {
	#print CAT "# $maxobj  $sortcat[0]\n";
	print CAT $cathash{$sortcat[0]}, "\n";
	$maxobj--;
	shift (@sortcat);
    }
    close (CAT);

    return $catname;
}


# Extract information from the NDF.  This sets the WCS domain to be
# SKY, and returns the original domain.  It also finds the SKY
# coordinates of the bounds of the NDF.
#
# Return a hash comprising
#
#    - {date}: the observation date;
#
#    - {x0}, {y0}, {x1}, {y1} are the x and y coordinates of the
#    opposite corners of the image.  Typically, we'll have {x0}<{x1}
#    and {y0}<{y1}, but there's no need to guarantee this.
#
#    - {wcs}: a reference to an array containing the lines of the WCS
#    information extracted from the NDF, or undef if there is none available;
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
sub ndf_info ($$$) {
    my ($NDF, $helpers, $tempdir) = @_;

    my $Kappa = $helpers->{kappa};
    my $NDFPack = $helpers->{ndfpack};

    my ($origdomain, @lboundpix, @uboundpix);
    my $okstatus = &Starlink::ADAM::SAI__OK;

    my $status = $NDFPack->obeyw ("ndftrace", "$NDF quiet=true");
    ($status == &Starlink::ADAM::DTASK__ACTCOMPLETE)
      || die "Error running ndftrace";

    ($status, @lboundpix) = $NDFPack->get ("ndftrace", "lbound");
    ($status == $okstatus)
      || die "Error getting ndftrace/lbound";
    #print STDERR "lbound=@lboundpix\n";

    ($status, @uboundpix) = $NDFPack->get ("ndftrace", "ubound");
    ($status == $okstatus)
      || die "Error getting ndftrace/ubound";
    #print STDERR "ubound=@uboundpix\n";

    my $tfile = "$tempdir/wcsshow";
    if ($noregenerate && -e $tfile) {
	print STDERR "Reusing $tfile...\n";
    } else {
	$status = $NDFPack->obeyw ("wcsshow", "ndf=$NDF logfile=$tfile quiet=true full=-1");
	($status == &Starlink::ADAM::DTASK__ACTCOMPLETE)
	  || die "Error running wcsshow";

	push (@tempfiles, $tfile);
    }

    my $wcsref;
    my @domainlist;
    my $isSkyDomain = 0;
    if (open (WCS, "<$tfile")) {
	my @wcslines = ( );
	my $line;
	while (defined($line = <WCS>)) {
	    chomp $line;
	    $line =~ s/^ *//;
	    if (!$isSkyDomain
		&& ($line =~ /domain *= *"?sky"?/i || $line =~ /begin  *skyframe/i)) {
		$isSkyDomain = 1;
		print STDERR "SKY domain exists\n";
	    #if ($line =~ /Domain *= *([^ ]*)/) {
		#print STDERR "DOMAIN $1\n";
		#$isSkyDomain = 1 if ($1 =~ /sky/i);
		#print STDERR "Weh-hey!\n" if ($isSkyDomain);
	    #}
	    }
	    push (@wcslines, $line);
	}
	close (WCS);
	$wcsref = \@wcslines;
    } else {
	print STDERR "ndf_info: NDF $NDF does not appear to have a WCS component\n";
	$wcsref = undef;
    }

    my %returnhash;		# returned

    my $fitshash;
    ($fitshash, $status) = fits_read_header($NDF);
    if ($status != $okstatus) {
	print STDERR "ndf_info: Can't read FITS component\n";
	$fitshash = undef;
    }
    my $ndfdates = get_dates ($NDF, $helpers, $fitshash);

    # Attempt to obtain observation for ASTROM observation records
    # ASTROM Time record
    if (defined($ndfdates->{fst})) {
	my @sthms = split (/[^0-9]+/,$ndfdates->{fst});
	$returnhash{astromtime} = sprintf ("%d %f",
					   $sthms[0],
					   $sthms[1]+($sthms[2]/60));
	$returnhash{astromtimecomment} = "FITS ST";
    }
    if (defined ($returnhash{astromtime})) {
	printf STDERR ("ndf_info: ASTROM Time=%s from %s\n",
		      $returnhash{astromtime},
		      $returnhash{astromtimecomment});
    } else {
	print STDERR "ndf_info: Can't work out ASTROM Time record\n";
    }

    if (defined($fitshash)) {
	# ASTROM Obs record
	if (defined($fitshash->{SLATEL})) {
	    $returnhash{astromobs} = $fitshash->{SLATEL};
	    $returnhash{astromobscomment} = "FITS SLATEL";
	} elsif (defined($fitshash->{LATITUDE})
		 && defined($fitshash->{LONGITUD})) {
	    # Latitude and longitude in (decimal) degrees.  Note the
	    # FITS LONGITUD keyword appears to be west-positive,
	    # though this doesn't appear to be written down
	    # anywhere. ASTROM requires the longitude to be
	    # east-positive.
	    my $t = $fitshash->{LONGITUD};
	    my $ti = int($t);
	    my $ao = sprintf ("%d %f", $ti, abs(($t-$ti)*60.0));
	    $t = $fitshash->{LATITUDE} * -1.0;
	    $ti = int($t);
	    $ao .= sprintf (" %d %f", $ti, abs(($t-$ti)*60.0));
	    $ao .= sprintf (" %f", $fitshash->{HEIGHT})
	      if (defined($fitshash->{HEIGHT}));
	    $returnhash{astromobs} = $ao;
	    $returnhash{astromobscomment} = "FITS LAT/LON";
	}
	# We might alternatively use the OBSERVAT keyword which
	# (always?, sometimes?) contains an IRAF observatory
	# keyword. The list of these, though not the mappings to SLA
	# codes, is in the IRAF obsdb.dat file.  See
	# http://tdc-www.harvard.edu/iraf/rvsao/bcvcorr/obsdb.html
	if (defined($returnhash{astromobs})) {
	    printf STDERR ("ndf_info: ASTROM Obs=%s from %s\n",
			  $returnhash{astromobs},
			  $returnhash{astromobscomment});
	} else {
	    print STDERR "ndf_info: can't work out ASTROM Obs record\n";
	}

	# ASTROM Met record
	if (defined($fitshash->{TEMPTUBE})) {
	    $returnhash{astrommet} = sprintf ("%f",
					      $fitshash->{TEMPTUBE} + 273.15);
	    $returnhash{astrommetcomment} = "FITS TEMPTUBE";
	}
	if (defined($returnhash{astrommet})) {
	    printf STDERR ("ndf_info: ASTROM Met=%s from %s\n",
			  $returnhash{astrommet},
			  $returnhash{astrommetcomment});
	} else {
	    print STDERR "ndf_info: can't work out ASTROM Met record\n";
	}

	# ASTROM Colour record
	if (defined($fitshash->{WFFBAND})) {
	    my $t = uc($fitshash->{WFFBAND});
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
	if (defined($returnhash{astromcol})) {
	    printf STDERR ("ndf_info: ASTROM Col=%s from %s\n",
			  $returnhash{astromcol},
			  $returnhash{astromcolcomment});
	} else {
	    print STDERR "ndf_info: can't work out ASTROM Col record\n";
	}
    }

    my $obsdate;
    if (defined($ndfdates->{ndd})) {
	$obsdate = $ndfdates->{ndd};
	print STDERR "ndf_info: obsdate $obsdate from NDF history\n";
    } elsif (defined($ndfdates->{fdd})) {
	$obsdate = $ndfdates->{fdd};
	print STDERR "ndf_info: obsdate $obsdate from FITS\n";
    } else {
	my @now = localtime();
	$obsdate = ymd2dec ($now[5]+1900, $now[4]+1, $now[3]);
	print STDERR "ndf_info: WARNING obsdate $obsdate taken to be NOW\n";
    }

    $returnhash{date} = $obsdate;
    $returnhash{x0} = $lboundpix[0];
    $returnhash{y0} = $lboundpix[1];
    $returnhash{x1} = $uboundpix[0];
    $returnhash{y1} = $uboundpix[1];
    $returnhash{wcs} = $wcsref;
    $returnhash{hassky} = $isSkyDomain;

    return \%returnhash;
}


# Given the name of an NDF file, try to extract an observation date
# from it, or any FITS file included within it.  Return a hash
# containing as many as possible of the fields njd, fjd (julian date
# from NDF or FITS component), ndd, fdd (decimal date -- years AD,
# from NDF or FITS), today (decimal date), nst, fst (sidereal time of obs,
# hh:mm:ss.frac).
sub get_dates ($$$) {
    my ($NDFfn, $helpers, $fitshash) = @_;
    my $okstatus = &NDF::SAI__OK;
    my $status = $okstatus;
    my $obsdate = undef;
    my %dates = ();

    # First look to see if this NDF has a history component.
    ndf_begin();
    my ($indf,$hashist);
    ndf_find  (NDF::DAT__ROOT(), $NDFfn, $indf, $status);
    ndf_state ($indf, 'History', $hashist, $status);
    if ($status == $okstatus) {
	if ($hashist) {
	    # Yes, it has.  Now find the last history record
	    my ($nhist, $hrec);
	    ndf_hinfo ($indf, 'NRECORDS', 0, $nhist, $status);
	    print STDERR "NDF $NDFfn has $nhist history records\n";
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
	    ($mnum <= 12) || carp "get_date: Unknown month $mstr\n";

	    $dates{ndd} = ymd2dec ($dates[0], $mnum, $dates[2]);
	}
    }

    # End the NDF context
    ndf_end ($status);

    if ($status != $okstatus) {
	print STDERR "get_date: NDF error (discarded)\n";
	# Reset the status
	$status = $okstatus;
    }

    if (defined($fitshash)) {
	if ($status != &NDF::SAI__OK) {
	    print STDERR "fits_read_header appeared to fail status=$status\n";
	} elsif (defined($$fitshash{'JD'})) {
	    $dates{fjd} = $$fitshash{'JD'};
	    print STDERR "fits_read_header{JD}=", $dates{fjd}, "\n";
	    # Convert JD to a decimal year.
	    # Julian epoch=J2000.0 + (JD - 2 451 545)/365.25
	    $dates{fdd} = 2000.0 + ($dates{fjd} - 2451545)/365.25;
	} elsif (defined($$fitshash{'DATE-OBS'})) {
	    my $fitsdate = $$fitshash{'DATE-OBS'};
	    printf STDERR "fits_read_header{DATE-OBS}=%s\n", $fitsdate;
	    
	    # Parse the yyyy?mm?dd date and convert it to a decimal year.
	    my ($datey,$datem,$dated,$junk);
	    ($datey,$datem,$dated,$junk) = split (/[^0-9]+/,$fitsdate,4);
	    if (defined($datem)) {
		defined($dated) || ($dated = 1);
		$dates{fdd} = ymd2dec ($datey, $datem, $dated);
	    }	
	}

	if (defined($fitshash->{ST})) {
	    $dates{fst} = $fitshash->{ST};
	} elsif (defined($fitshash->{STSTART})) {
	    $dates{fst} = $fitshash->{STSTART};
	}
    }

#    unless (defined ($obsdate)) {
#	# Don't know -- just say NOW.
#	my @now = localtime();
#	$obsdate = ymd2dec ($now[5]+1900, $now[4]+1, $now[3]);
#    }

    return \%dates;
}


# Invoke the moggy server to obtain a catalogue corresponding to the
# proffered bounds of the NDF.  Return the name of a file which is
# suitable for input into FINDOFF.
sub get_catalogue ($\%$$) {
    my ($cat, $NDFinforef, $maxobj, $tempdir) = @_;

    my $mytempfile = "$tempdir/catalogue";

    if ($noregenerate && -e $mytempfile) {
	print STDERR "Reusing $mytempfile...\n";
	return $mytempfile;
    }

    my @wcs = @{$NDFinforef->{wcs}};

    # Pass the WCS information to moggy, declaring that future points
    # will be specified in the pixel domain.
    $cat->astinformation ('pixel', @wcs);

    # Determine the bounds of the image, and make a query of all the
    # objects sitting in a box with these points at opposite
    # corners. In fact, ask for a box somewhat larger than this (say,
    # 10% in linear dimension), anticipating some misalignment in the
    # initial astrometry.  The code below appears specific to the
    # case (p1=lower-left, p2=upper-right), but it isn't, in fact.
    #
    # XXX should we make this margin of 10% configurable?  Probably
    # not -- it doesn't matter here if the projection pole is off the
    # plate, as long as the centre of _this_ plate is reasonably accurate.
    my $p1x = $NDFinforef->{x0};
    my $p1y = $NDFinforef->{y0};
    my $p2x = $NDFinforef->{x1};
    my $p2y = $NDFinforef->{y1};
    my $sizex = $p2x-$p1x;
    my $sizey = $p2y-$p1y;
    my $p1xq = $p1x-0.1*$sizex;
    my $p1yq = $p1y-0.1*$sizey;
    my $p2xq = $p2x+0.1*$sizex;
    my $p2yq = $p2y+0.1*$sizey;
    $cat->searchtype('box');
    $cat->point     ($p1xq, $p1yq);
    $cat->otherpoint($p2xq, $p2yq);

    # Get a decent number of points
    $cat->maxrow($maxobj);

    $cat->status_ok()
      || die "Can't set catalogue parameters ("
	. $cat->current_statusmessage() . ")\n";

    $cat->query()
      || die "Can't make catalogue query ("
	. $cat->current_statusmessage() . ")\n";

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
      || die "Can't find expected column ($xcol,$ycol,$racol,$deccol) in catalogue results\n";
    open (PIXCAT, ">$mytempfile")
      || die "Can't open PIXCAT $mytempfile to write\n";
    push (@tempfiles, $mytempfile);
    print PIXCAT "# Catalogue generated from autoastrom.pl\n";
    my $i;
    for ($i=0; $i<$nrows; $i++) {
#	for ($j=0; $j<=$#collist; $j++) {
#	    print PIXCAT "\t", $resref->[$i]->[$collist[$j]];
#	}
#	print PIXCAT "\n";
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
                                # (may include {error}, {maxdisp}, {complete})
    my $tempfn = shift;		# Temporary filename prefix


    my $cat1out = "$tempfn-cat1.out";
    my $cat2out = "$tempfn-cat2.out";
    if ($noregenerate && -e $cat1out && -e $cat2out) {
	print STDERR "Reusing $cat1out and $cat2out...\n";
	return ($cat1out, $cat2out);
    }

    # Check that the input files exist
    (-r $cat1 && -r $cat2)
      || die "Can't find FINDOFF input files $cat1 and $cat2\n";

    # Write these two file names to an input file for FINDOFF
    my $findoffinfile = "$tempfn-findoffin";
    open (FOFF, ">$findoffinfile")
      || die "Can't open file $findoffinfile to write\n";
    print FOFF "$cat1\n$cat2\n";
    close (FOFF);
    push (@tempfiles, $findoffinfile);

    # ... and a file containing the names of the output file
    my $findoffoutfile = "$tempfn-findoffout";
    open (FOFF, ">$findoffoutfile")
      || die "Can't open file $findoffoutfile to write\n";
    print FOFF "$cat1out\n$cat2out\n";
    close (FOFF);
    push (@tempfiles, $findoffoutfile);

    push (@tempfiles, $cat1out);
    push (@tempfiles, $cat2out);


    # Notes:
    #   ndfnames is false, so restrict and usewcs are ignored.
    #   error=1 is the default.
    #   minsep is unspecified -- default is 5*error.
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
    $findoffarg .= " error=".(defined($foffopts->{error})
			      ? $foffopts->{error}
			      : 5);
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

    print STDERR "Calling findoff\n";
    my $e;
    foreach $e (split(' ',$findoffarg)) {
	print STDERR "\t$e\n";
    }

#    my $proc = $ccdpack->pid();
#    print STDERR "...process PID is ", $proc->pid(), ".  Running? ",
#        ($proc->poll() ? "yes" : "no"), "\n";
    $ccdpack->contact()
      || croak "Ooops, can't contact the CCDPACK monolith\n";
    my $status = $ccdpack->obeyw ("findoff", $findoffarg);

    # XXX distinguish the case where FINDOFF crashes somehow, from the
    # case where it fails to find matches, and DO NOT die in the
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
	croak "Oh dear, it looks like the CCDPACK monolith has just died!\n(did I do that...?)";
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

    foreach my $k qw{CCDin catalogue findofferror helpers NDFinfo tempfn} {
	defined($par->{$k})
	  || croak "bad call to generate_astrom: parameter $k not specified\n";
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
      || die "Can't open file $par-{CCDin} to read\n";
    my $lineno = 0;
    my $line;
    my @CCDarray = ();
    while (defined($line = <IN>)) {
	next if ($line =~ /^ *\#/);
	$lineno++;
	my @tmparr = split (' ', $line);
	$tmparr[0] == $lineno || die "File $par->{CCDin} has wrong format\n";
	push (@CCDarray, \@tmparr);
    }
    close (IN);

    my @CATarray = ();
    open (IN, "<$par->{catalogue}")
      || die "Can't open file $par->{catalogue} to read\n";
    $lineno = 0;
    while (defined($line = <IN>)) {
	next if ($line =~ /^ *\#/);
	$lineno++;
	my @tmparr = split (' ', $line);
	$tmparr[0] == $lineno
	  || die "File $par->{catalogue} has wrong format\n";
	push (@CATarray, \@tmparr);
    }
    close (IN);

    ($#CCDarray == $#CATarray)
      || die "generate_astrom: input files have different number of matches\n";


    # {maxnterms} is the maximum number of fit terms to try, and
    # $nterms is the number we will try in fact.  Unless we have more
    # than 10 reference stars _and_ adequate Observation Data, $nterms
    # should be at most 6.
    my $nterms = (defined($par->{maxnterms}) ? $par->{maxnterms} : 6);
    if ($nterms > 6 && ! (defined($par->{NDFinfo}->{astromtime})
			  && defined($par->{NDFinfo}->{astromobs})
			  && defined($par->{NDFinfo}->{astromcol})
			  && ($#CCDarray >= 10))) {
	print STDERR "generate_astrom: Not enough observation data, I'm doing only a 6-parameter fit\n";
	$nterms = 6;
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
		   $xoffset, $yoffset, $statM, $samplesd);

    #+ How good a match is this?
    #
    # The quantities $S^2_x=M_x/(n-1)$ and $S^2_y=M_y/(n-1)$ are the sample
    # variances of the $x$ and $y$ variables.  The variables
    # $M_x/\sigma^2$ and $M_y/\sigma^2$ each have a $\chi^2$
    # distribution with $n-1$ degrees of freedom, and hence
    # $M/\sigma^2$ has a $\chi^2$ distribution with $2(n-1)$ d.o.f.
    #
    # %Let us take the mean offset (|$xoffset,$yoffset|) as exact and
    # %examine the residuals (|$xoffs[]-$xoffset|) and
    # %(|$yoffs[]-$yoffset|).  We don't really know the distribution of
    # %the errors in $x$ and $y$, but we can guess it might be normal
    # %($N(0,\sigma)$).  That means that the variable
    # %$Z_i=(x_i^2+y_i^2)/\sigma^2$ has a $\chi^2$ distribution with two
    # %degrees of freedom.  This means in turn that the statistic
    # %$M=\sum^n Z_i$ also has a $\chi^2$ distribution, but with $2n$
    # %degrees of freedom.
    #
    # We don't know what the $x$ and $y$ standard deviations are,
    # other than that we suppose them to be equal, but we can estimate
    # them by supposing that the FINDOFF error box contains some
    # fraction $p_e$ of the points (take this box to be of side $2e$,
    # where $e$ is the ERROR parameter for FINDOFF, passed as argument
    # findofferr).  We have $p_e = Prob(|x|<e \& |y|<e)$, or
    # $p_e=(P(e/\sigma)-P(-e/\sigma))^2 = [2P(e/\sigma)-1]^2$, where
    # $P(x)$ is the cumulative probability distribution for the
    # standard Normal distribution.  Thus $P(e/\sigma)=(\sqrt{p_e}+1)/2$, and
    # \begin{center}
    # \catcode`\|=12
    # \begin{tabular}{r|ccccccc}
    # $p_e$      & 0.5  & 0.6  & 0.7  & 0.8  & 0.9  & 0.95 & 0.99 \\
    # \hline
    # $e/\sigma$ & 1.06 & 1.21 & 1.39 & 1.62 & 1.95 & 2.23 & 2.81 \\
    # \end{tabular}\end{center}
    # We can also calculate this on the fly, using subroutine invP:
    # $e/\sigma=\mbox{\texttt{invP}}(p_e)$
    #
    # That is, the analysis below checks the hypothesis that the values
    # of $p_e$, $e$ and $\sigma$ are consistent.  We choose $p_e$, and 
    # hence, from the above calculation, implicitly choose
    # $e/\sigma$, so it's really testing whether the selected
    # value of $e$ is appropriate.
    #
    # In principle we should also worry about the errors in the
    # offset, but these are down on $\sigma$ by a factor of $n$ or so,
    # so as long as we have more than about three matches, we can
    # ignore them.
    #
    # Do note, however, that all this is not really worth sweating
    # about, since the point of this program is that we have unknown
    # systematic errors in the CCD positions, which it is this
    # program's task to find out.  However, this is about the only way
    # we have of dynamically adjusting FINDOFF's `error' parameter, so
    # it is worth trying to do \emph{something} sensible.
    #
    #-
#    my $statM = 0;
#    for ($i=0; $i<$nmatches; $i++) {
#	my $xo = $xoffs[$i]-$xoffset;
#	my $yo = $yoffs[$i]-$yoffset;
#	$statM += ($xo*$xo + $yo*$yo);
#	printf STDERR "  (%9.5f, %9.5f) %f\n", $xo, $yo, $statM;
#    }
#    printf STDERR "-- statM=%f\n", $statM;

    # Debugging/exploration...
    #    printf "--- %2s %3s%10.3f%10.3f%10.3f%10.3f\n", "e", "dof", 1.06,
    # 1.95, 2.23, 2.81; printf "--- %2s
    # %3s%10.3f%10.3f%10.3f%10.3f\n", "-", "---", 0.5, 0.9, 0.95,
    # 0.99; foreach $serr ((1, 3, 5, 7, 9)) { printf "--- %2d %3d",
    # $serr, 2*$nmatches; foreach $mult ((1.06, 1.95, 2.23, 2.81)) {
    # my $sig = $serr/$mult; printf "%10.3f(%5.3f)",
    # $statM/($sig*$sig), qchi($statM/($sig*$sig), 2*$nmatches); }
    # print "\n"; } 

    # Get Q(\chi) for this value of M.
    my $findoffboxprop = (defined($par->{findoffboxprop})
			  ? $par->{findoffboxprop}
			  : 0.5); # The proportion of points expected
                                  # to be in the FINDOFF error box.
    my $eoversigma = invP($findoffboxprop);
    my $findofferr = $par->{findofferror};
    #+ |$findoffsigma| is the value of $\sigma$ that would result in a
    # fraction |$findoffboxprop| of the points inside the FINDOFF
    # error box.
    #-
    my $findoffsigma = $findofferr/$eoversigma;
    my $Q = qchi ($statM/($findoffsigma*$findoffsigma), 2*($nmatches-1));
    my $matchquality;
    # Test the quality of the match using a two-tailed test at p=0.99
    # and p=0.95 levels of significance.  That is, compare Q with
    # 0.5+-(0.99/2) and 0.5+-(0.95/2)
  MATCHQUALITY: {
	if ($Q > 0.995) {
	    # An implausibly good match
	    printf STDERR ("Very poor (way too good) match\n\t(%d matches, SSD=%f, Q(chi2)=%.5f).\n\tThe FINDOFF error parameter is probably too high.\n\t(findoff errorbox size=%g, prop=%.0f%%, SExtractor sigma=%.2f pixels, Q(samplesd)=%.5f)\n",
			   $nmatches, $samplesd, $Q,
			   $findofferr, $findoffboxprop*100, $findoffsigma,
			   qchi($statM/($samplesd*$samplesd),2*($nmatches-1)));
	    $matchquality = +3;
	    last MATCHQUALITY;
	}
	if ($Q > 0.975) {
	    # A dubiously good match
	    printf STDERR ("Rather dodgy (ie, implausibly good) match\n\t(%d matches, SSD=%f, Q(chi2)=%.3f).\n\tThe FINDOFF error parameter may be too high.\n\t(findoff errorbox size=%g, prop=%.0f%%, SExtractor sigma=%.2f pixels)\n",
			   $nmatches, $samplesd, $Q,
			   $findofferr, $findoffboxprop*100, $findoffsigma);
	    $matchquality = +2;
	    last MATCHQUALITY;
	}
	if ($Q > 0.025) {
	    # A good match, at p=0.95
	    printf STDERR ("Good match\n\t(%d matches, SSD=%f, Q(chi2)=%.3f).\n\t(findoff errorbox size=%g, prop=%.0f%%, SExtractor sigma=%.2f pixels)\n",
			   $nmatches, $samplesd, $Q,
			   $findofferr, $findoffboxprop*100, $findoffsigma);
	    if ($Q > 0.5) {
		$matchquality = +1;
	    } else {
		$matchquality = -1;
	    }
	    last MATCHQUALITY;
	}
	if ($Q > 0.005) {
	    # A dubiously poor match
	    printf STDERR ("Rather dodgy (ie, implausibly bad) match\n\t(%d matches, SSD=%f, Q(chi2)=%.5f).\n\tEither these position lists do not match,\n\tor else the FINDOFF error parameter is set too low.\n\t(findoff errorbox size=%g, prop=%.0f%%, SExtractor sigma=%.2f pixels)\n",
			   $nmatches, $samplesd, $Q,
			   $findofferr, $findoffboxprop*100, $findoffsigma);
	    $matchquality = -2;
	    last MATCHQUALITY;
	}
	# Q<=0.025.  A very poor match
	printf STDERR ("Unlikely (ie, implausibly bad) match\n\t(%d matches, SSD=%f, Q(chi2)=%.3f).\n\tEither these position lists simply do not match,\n\tor else the FINDOFF error parameter is far too low.\n\t(findoff errorbox size=%g, prop=%.0f%%, SExtractor sigma=%.2f pixels)\n",
		       $nmatches, $samplesd, $Q,
		       $findofferr, $findoffboxprop*100, $findoffsigma);
	$matchquality = -3;
    }
    # e/\sigma=$eoversigma, so given that $samplesd is an estimate of
    # \sigma, estimate the value of e which is consistent with the
    # $eoversigma we guessed above.
    my $suggestedfindofferr = $samplesd * $eoversigma;
	

    my $astromofile = "$tempfn-astromin";
    open (ASTROMOUT, ">$astromofile")
      || die "Can't open file $astromofile to write\n";
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
	#my ($pcx, $pcy);  XXX tidy up
	#my @ndfbound = @{$par->{NDFinfo}->{limits}};
	#$pcx = ($ndfbound[0] + $ndfbound[2])/2.0;
	#$pcy = ($ndfbound[1] + $ndfbound[3])/2.0;
	my $pcx = ($par->{NDFinfo}->{x0} + $par->{NDFinfo}->{x1})/2.0;
	my $pcy = ($par->{NDFinfo}->{y0} + $par->{NDFinfo}->{y1})/2.0;
	if (defined($astrom) && defined($astrom->{wcs})) {
	    # Do the calculation by getting the plate centre in pixels,
	    # and converting this to SKY coordinates using ATOOLS/asttrann
	    my @row = ($pcx, $pcy);
	    my @dat = (\@row);
	    my $inndfname = twodarray2ndf (@dat, $par->{tempfn}.'-coordtrans');
	    my $outndfname = "$inndfname-out";
	    my $asttranarg = "this=$astrom->{wcs}";
	    $asttranarg .= " in=$inndfname out=$outndfname forward=true";
	    print STDERR "Calling asttrann $asttranarg...\n";
	    my $status = $par->{helpers}->{atools}->obeyw("asttrann",
							  $asttranarg);
	    ($status == &Starlink::ADAM::DTASK__ACTCOMPLETE)
	      || carp "generate_astrom: error running asttrann";
	    push (@tempfiles, ("$outndfname.sdf", "$inndfname.sdf"));
	    my $skyd = ndf2twodarray ($outndfname);
	    @projpole = @{$skyd->[0]};
	    $projpole[0] *= $d2r;
	    $projpole[1] *= $d2r;
	    print STDERR "Plate centre: ($pcx,$pcy)->($bestastrometry)->($projpole[0],$projpole[1])\n";
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
	      || croak "generate_astrom: Can't convert coordinates";
	    @projpole = @$projpoleref;
	    print STDERR "Plate centre: offset ($xoffset,$yoffset): ($pcx,$pcy)->($projpole[0],$projpole[1])\n";
	}
	$projpolesex[0] = dmshms ($projpole[0], 1);
        $projpolesex[1] = dmshms ($projpole[1], 0);
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
	    dmshms($CATarray[$i]->[3], 1),
	    dmshms($CATarray[$i]->[4], 0),
	    $CCDarray[$i]->[4], $CATarray[$i]->[5];
	printf ASTROMOUT "%12f   %12f\n", $CCDarray[$i]->[1],
	    $CCDarray[$i]->[2];
    }

    print ASTROMOUT "END\n";
    close (ASTROMOUT);

    return {filename => $astromofile,
	    nmatches => $nmatches,
	    quality => $matchquality,
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
		   $alog);
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
		  #print STDERR "run_astrom: RESULT $1=$2\n";
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
#    while (defined ($l=<SUM>)) {
#	chop $l;
#	($l =~ /^FIT/) && do { $resultline=undef; $statline=undef; next; };
#
#	($l =~ /^RESULT *(.*)/) && do { $resultline = $1; next; };
#
#	($l =~ /^STAT *(.*)/) && do { $statline = $1; next; };
#
#	($l =~ /^ENDFIT/) && do {
#	    unless (defined($statline) && defined($resultline)) {
#		print STDERR "ASTROM logfile malformed!\n";
#		next;
#	    }
#	    # STAT line has fields:
#	    #    nstars, x-rms, y-rms, r-rms, pix-sum-sq
#	    my @stats = split (' ', $statline);
#	    # RESULT line has fields:
#	    #    nterms, q, RA-rad, DEC-rad, RA-sex, DEC-sex, FITS-WCS
#	    my @result = split (' ', $resultline);
#	    my %t = ();
#	    $t{nterms} = $result[0];
#	    $t{q}      = $result[1];
#	    $t{rarad}  = $result[2];
#	    $t{decrad} = $result[3];
#	    $t{rasex}  = $result[4];
#	    $t{decsex} = $result[5];
#	    $t{wcs}    = $result[6];
#	    push (@tempfiles, $t{wcs}) if (defined($t{wcs}));
#	    $t{nstars} = $stats[0];
#	    $t{residual} = $stats[4];
#	    push (@ret, \%t);
#	    next;
#	};
#    }
    close (SUM);

    @ret || print STDERR "run_astrom: no return values! Either astrom failed, or the log file was incomplete\n";

    return @ret;
}



# Given decimal degrees, return a string containing (space-separated)
# DMS or HMS, depending on whether the second argument is true or
# false.  
sub dmshms ($$) {
    my ($val, $isra) = @_;
    my ($dh, $min, $sec, $mas, $sign);
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
	    die "DEC of $val out of range\n";
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
    return sprintf ("%d %2d %2d.%03d", $dh, $min, $sec, $mas);
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
	print STDERR "twodarray2ndf: inconsistent array dimensions: ",
	    "row $i has ".$#{$dat[$i]}." entries, not ".$#{$dat[0]}."\n";
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

# Convert year,month,day to a decimal year.  Very simple, but accurate enough.
# Assumes input parameters are within valid ranges.
sub ymd2dec ($$$) {
    my ($year,$month,$day) = @_;
    my @months = (0,
		  31, 31, 28, 30, 31, 30,
		  31, 31, 30, 31, 30, 31);
    my $totday = 0;
    my $i;
    for ($i=1; $i<$month; $i++) {
	$totday += $months[$i];
    }
    $totday += $day;
    return $year + $totday/365.25;
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

1;
