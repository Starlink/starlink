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
sub ndf_info ($$$$);
sub get_catalogue ($\@$$);
sub match_positions ($$$$$);
sub generate_astrom ($$$$\@$$$);
sub run_astrom ($$);
sub dmshms ($$);
sub qchi ($$);
sub canonicalise_ndfname ($);
sub twodarray2ndf (\@$);
sub ndf2twodarray ($);
sub txt2arr ($);
sub txt2ndf ($;$);
sub ndf2txt ($;$);
sub reuse_files (;$);
sub get_temp_files ();


my $noregenerate = 0;
my @tempfiles = ();


sub extract_objects ($$$$) {
    my ($extractor, $ndfname, $maxobj, $tempdir) = @_;
    # Extract objects from the NDF file $ndfname, returning the
    # filename of the resulting catalogue, using $tempdir as a path for
    # temporary files, and returning a maximum of $maxobj objects.
    # The file which is returned should be suitable for input to
    # FINDOFF.  On error, return undef.


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

    my $parlist = "image=$ndfname config=$ENV{AUTOASTROM_DIR}/misc/extractor.config keywords=false";
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
# Return a three-element array comprising
#
#    - the observation date;
#
#    - a reference to an array containing the lines of the WCS
#    information extracted from the NDF;
#
#    - a reference to an array containing the plus the PIXEL
#    coordinates of the lower and upper corners of the image (lower-x,
#    lower-y, upper-x, upper-y).
#
sub ndf_info ($$$$) {
    my ($NDF, $Kappa, $NDFPack, $tempdir) = @_;

    my ($origdomain, @lboundpix, @uboundpix);

    my $status = $NDFPack->obeyw ("ndftrace", "$NDF quiet=true");
    ($status == &Starlink::ADAM::DTASK__ACTCOMPLETE)
      || die "Error running ndftrace";

    ($status, @lboundpix) = $NDFPack->get ("ndftrace", "lbound");
    ($status == &Starlink::ADAM::SAI__OK)
      || die "Error getting ndftrace/lbound";
    #print STDERR "lbound=@lboundpix\n";

    ($status, @uboundpix) = $NDFPack->get ("ndftrace", "ubound");
    ($status == &Starlink::ADAM::SAI__OK)
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

    open (WCS, "<$tfile") || die "Can't open WCS dump $tfile to read";
    my @wcslines = ( );
    my $line;
    while (defined($line = <WCS>)) {
	chomp $line;
	$line =~ s/^ *//;
	push (@wcslines, $line);
    }
    close (WCS);

    my @corners = ($lboundpix[0], $lboundpix[1], $uboundpix[0], $uboundpix[1]);
    my $obsdate = get_date($NDF);

    return ($obsdate, \@wcslines, \@corners);
}


# Given the name of an NDF file, try to extract an observation date from it.
sub get_date ($) {
    my $NDFfn = shift;
    my $fitshash;
    my $status;
    my $obsdate = undef;

    ($fitshash, $status) = fits_read_header($NDFfn);
    if ($status != &NDF::SAI__OK) {
	print STDERR "fits_read_header appeared to fail status=$status\n";
    } else {
	my $fitsdate = $$fitshash{'DATE-OBS'};
	printf STDERR "fits_read_header{DATE-OBS}=%s\n", $fitsdate;

	# Parse the yyyy?mm?dd date and convert it to a decimal year.
	# We don't have to be particularly fussy here.
	my ($datey,$datem,$dated,$junk);
	($datey,$datem,$dated,$junk) = split (/[^0-9]+/,$fitsdate,4);
	if (defined($datem)) {
	    defined($dated) || ($dated = 1);

	    $obsdate = sprintf ("%.2f",
				$datey + ($datem-1)/12 + ($dated-1)/365.5);
	}	
    }

    unless (defined ($obsdate)) {
	# Don't know -- just say NOW.
	my @now = localtime();
	$obsdate = sprintf ("%.2f", $now[5] + $now[7]/365.5);
    }

    return $obsdate;
}


# Invoke the moggy server to obtain a catalogue corresponding to the
# proffered bounds of the NDF.  Return the name of a file which is
# suitable for input into FINDOFF.
sub get_catalogue ($\@$$) {
    my ($cat, $NDFinforef, $maxobj, $tempdir) = @_;

    my $mytempfile = "$tempdir/catalogue";

    if ($noregenerate && -e $mytempfile) {
	print STDERR "Reusing $mytempfile...\n";
	return $mytempfile;
    }

    # NDFinforef is (date, wcs-array, ndfbound-array)
    my @wcs = @{$NDFinforef->[1]};
    my @ndfbound = @{$NDFinforef->[2]};

    # Pass the WCS information to moggy, declaring that future points
    # will be specified in the pixel domain.
    $cat->astinformation ('pixel', @wcs);

    # Determine the bounds of the image, and make a query of all the
    # objects sitting in a box with these points at opposite
    # corners. In fact, ask for a box somewhat larger than this (say,
    # 10% in linear dimension), anticipating some misalignment in the
    # initial astrometry.  The code below appears specific to the
    # case (p1=lower-left, p2=upper-right), but it isn't, in fact.
    my $p1x = $ndfbound[0];
    my $p1y = $ndfbound[1];
    my $p2x = $ndfbound[2];
    my $p2y = $ndfbound[3];
    my $sizex = $p2x-$p1x;
    my $sizey = $p2y-$p1y;
    my $p1xq = $p1x-0.1*$sizex;
    my $p1yq = $p1y-0.1*$sizey;
    my $p2xq = $p2x+0.1*$sizex;
    my $p2yq = $p2y+0.1*$sizey;
    $cat->searchtype('box');
    $cat->point     ($p1xq, $p1yq);
    $cat->otherpoint($p2xq, $p2yq);
    #$cat->point($ndfbound[0], $ndfbound[1]);
    #$cat->otherpoint($ndfbound[2], $ndfbound[3]);

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
# results, in the same order as the corresponding input files.
sub match_positions ($$$$$) {
    my ($ccdpack, $cat1, $cat2, $sexerr, $tempfn) = @_;


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
    my $findoffarg = "ndfnames=false inlist=^$findoffinfile outlist=^$findoffoutfile logto=logfile logfile=$tempfn-findofflog error=$sexerr usecomp=false fast=true failsafe=true maxdisp=!";
    print STDERR "Starting FINDOFF ($findoffarg)...\n";

#    my $proc = $ccdpack->pid();
#    print STDERR "...process PID is ", $proc->pid(), ".  Running? ",
#        ($proc->poll() ? "yes" : "no"), "\n";
    $ccdpack->contact()
      || die "Ooops, can't contact the FINDOFF monolith\n";
    my $status = $ccdpack->obeyw ("findoff", $findoffarg);
    print STDERR "...finished FINDOFF\n";

    $status == &Starlink::ADAM::DTASK__ACTCOMPLETE
      || die "Error running CCDPack/findoff";

    push (@tempfiles, "$tempfn-findofflog");

    return ($cat1out, $cat2out);
}


# Given two FINDOFF output files and a tempfilename, generate an ASTROM input
# file.  Return an array containing the file name, the number of
# matches, and a code indicating the quality of the match (see below).
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
# The third return value is a code indicating the quality of the
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
sub generate_astrom ($$$$\@$$$) {
    my $CCDin = shift;		# SExtractor output file -- CCD positions
    my $CATin = shift;		# Catalogue
    my $sexerr = shift;		# Size of the error parameter used for FINDOFF
    my $moggy = shift;		# Reference to the moggy application
    my $NDFinforef = shift;	# Pixel bounds of the input NDF
    my $ATOOLS = shift;		# Reference to ATOOLS monolith. Needed for...
    my $bestastrometry = shift;	# Best astrometry so far, as a FITS file, 
				# or undef if none yet
    my $tempfn = shift;		# Prefix for temporary filenames

    my $astromofile = "$tempfn-astromin";

    # First, read the two files in.  CCDin first.  The first columns
    # should be identical, being a continuous sequence of integers,
    # starting with 1, so check this on input.
    open (IN, "<$CCDin") || die "Can't open file $CCDin to read\n";
    my $lineno = 0;
    my $line;
    my @CCDarray = ();
    while (defined($line = <IN>)) {
	next if ($line =~ /^ *\#/);
	$lineno++;
	my @tmparr = split (' ', $line);
	$tmparr[0] == $lineno || die "File $CCDin has wrong format\n";
	push (@CCDarray, \@tmparr);
    }
    close (IN);

    my @CATarray = ();
    open (IN, "<$CATin") || die "Can't open file $CATin to read\n";
    $lineno = 0;
    while (defined($line = <IN>)) {
	next if ($line =~ /^ *\#/);
	$lineno++;
	my @tmparr = split (' ', $line);
	$tmparr[0] == $lineno || die "File $CATin has wrong format\n";
	push (@CATarray, \@tmparr);
    }
    close (IN);

    ($#CCDarray == $#CATarray)
      || die "generate_astrom: input files have different number of matches\n";


    # Want to calculate $\bar x=(\sum x_i)/n$ and
    # $M_x=\sum(x_i-\bar x)^2 = \sum x_i^2 - (\sum x_i)^2/n$, and the
    # same for $(x\to y)$.  The vector $(\bar x,\bar y)$ is the mean
    # offset of the SExtractor points from the catalogue points, and
    # $M=M_x+M_y$ is the statistic we examine below.
    my $sumx = 0;
    my $sumy = 0;
    my $sumsq = 0;
    my ($xoffset,$yoffset,$i,$tmp);
    #my (@xoffs, @yoffs);
    my $nmatches = $#CCDarray + 1;
    $xoffset = $yoffset = 0;
    for ($i=0; $i<$nmatches; $i++) {
	#print STDERR "1=".${$CCDarray[$i]}[1]. " 2=". $CCDarray[$i]->[1]."\n";
	$tmp += ($CCDarray[$i]->[1] - $CATarray[$i]->[1]);
	#push (@xoffs, $tmp);
	#$xoffset += $tmp;
	$sumx += $tmp;
	$sumsq += $tmp*$tmp;
	#print STDERR $CCDarray[$i]->[1]." - ". $CATarray[$i]->[1] . " = $xoffset\n";
	$tmp = ($CCDarray[$i]->[2] - $CATarray[$i]->[2]);
	#push (@yoffs, $tmp);
	#$yoffset += $tmp;
	$sumy += $tmp;
	$sumsq += $tmp*$tmp;
    }
    $xoffset = $sumx/$nmatches;
    $yoffset = $sumy/$nmatches;
    printf STDERR "CCD-CAT offset=(%f,%f)pix, RMS=%fpix\n",
        $xoffset, $yoffset, sqrt($sumsq/$nmatches);

    # How good a match is this?
    #
    # Let us take the mean offset ($xoffset,$yoffset) as exact and
    # examine the residuals ($xoffs[]-$xoffset) and
    # ($yoffs[]-$yoffset).  We don't really know the distribution of
    # the errors in $x$ and $y$, but we can guess it might be normal
    # ($N(0,\sigma)$).  That means that the variable
    # $Z_i=(x_i^2+y_i^2)/\sigma^2$ has a $\chi^2$ distribution with two
    # degrees of freedom.  This means in turn that the statistic
    # $M=\sum^n Z_i$ also has a $\chi^2$ distribution, but with $2n$
    # degrees of freedom.
    #
    # We don't know what the $x$ and $y$ standard deviations are,
    # other than that we suppose them to be equal, but we can estimate
    # them by supposing that the FINDOFF error box contains some
    # fraction $p_e$ of the points (take this box to be of side $2e$,
    # where $e$ is the ERROR parameter for FINDOFF, passed as argument
    # sexerr).  We have $p_e = Prob(|x|<e \& |y|<e)$, or
    # $p_e=(P(e/\sigma)-P(-e/\sigma))^2 = [2P(e/\sigma)-1]^2$, where
    # $P(x)$ is the cumulative probability distribution for the
    # standard Normal distribution.  Thus
    # $P(e/\sigma)=(\sqrt{p_e}+1)/2$, and $p_e=0.5$, 0.9, 0.95, 0.99 give
    # $e/\sigma=1.06$, 1.95, 2.23 and 2.81.
    #
    # In principle we should also worry about the errors in the
    # offset, but these are down on $\sigma$ by a factor of $n$ or so,
    # so as long as we have more than about three matches, we can
    # ignore them.
    #
    # Do note, however, that all this is not worth sweating about,
    # since the point of this program is that we have unknown
    # systematic errors in the CCD positions, which it is this
    # program's task to find out.
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

    # Get Q(\chi) for this value of M.  Take it that the
    # FINDOFF error box contains 0.95 of the points.
    my $eoversigma = 2.23;
    my $statsigma = $sexerr/$eoversigma;
    my $statM = ($sumsq - $sumx*$sumx/$nmatches - $sumy*$sumy/$nmatches)
      / ($statsigma*$statsigma);
    my $Q = qchi ($statM, 2*$nmatches);
    my $matchquality;
    # Test the quality of the match using a two-tailed test at p=0.99
    # and p=0.95 levels of significance.
  MATCHQUALITY: {
	if ($Q > 0.995) {
	    # An implausibly good match
	    printf STDERR "Very poor (way too good) match (%d matches, RMS=%f, Q(chi2)=%.3f).\nThe FINDOFF error parameter is probably too high.\n(findoff error=%g, est. SExtractor sigma=%.2f pixels)\n",
	        $nmatches, sqrt($sumsq/$nmatches), $Q, $sexerr, $statsigma;
	    $matchquality = +3;
	    last MATCHQUALITY;
	}
	if ($Q > 0.975) {
	    # A dubiously good match
	    printf STDERR "Rather dodgy (ie, implausibly good) match (%d matches, RMS=%f, Q(chi2)=%.3f).\nThe FINDOFF error parameter may be too high.\n(findoff error=%g, est. SExtractor sigma=%.2f pixels)\n",
	        $nmatches, sqrt($sumsq/$nmatches), $Q, $sexerr, $statsigma;
	    $matchquality = +2;
	    last MATCHQUALITY;
	}
	if ($Q > 0.025) {
	    # A good match, at p=0.95
	    printf STDERR "Good match (%d matches, RMS=%f, Q(chi2)=%.3f).\n(findoff error=%g, est. SExtractor sigma=%.2f pixels)\n", $nmatches, sqrt($sumsq/$nmatches), $Q, $sexerr, $statsigma;
	    if ($Q > 0.5) {
		$matchquality = +1;
	    } else {
		$matchquality = -1;
	    }
	    last MATCHQUALITY;
	}
	if ($Q > 0.005) {
	    # A dubiously poor match
	    printf STDERR "Rather dodgy (ie, implausibly bad) match (%d matches, RMS=%f, Q(chi2)=%.3f).\nEither these position lists do not match,\nor else the FINDOFF error parameter is set too low.\n(findoff error=%g, est. SExtractor sigma=%.2f pixels)\n",
	        $nmatches, sqrt($sumsq/$nmatches), $Q, $sexerr, $statsigma;
	    $matchquality = -2;
	    last MATCHQUALITY;
	}
	# Q<=0.025.  A very poor match
	printf STDERR "Unlikely (ie, implausibly bad) match (%d matches, RMS=%f, Q(chi2)=%.3f).\nEither these position lists simply do not match,\nor else the FINDOFF error parameter is far too low.\n(findoff error=%g, est. SExtractor sigma=%.2f pixels)\n",
	    $nmatches, sqrt($sumsq/$nmatches), $Q, $sexerr, $statsigma;
	$matchquality = -3;
    }
	

    open (ASTROMOUT, ">$astromofile")
      || die "Can't open file $astromofile to write\n";
    push (@tempfiles, "$astromofile");
    my $timestring = localtime();
    print ASTROMOUT "* ASTROM input file generated by...\n* $0\n* $timestring\n";
    print ASTROMOUT "* Star references are SExtractor/Catalogue numbers\n";
    print ASTROMOUT "J2000\nASTR\n";

    # Determine the plate centre.  If the argument $bestastrometry is
    # defined, then it holds a FITS file with the best astrometry so
    # far.  If not, then the best we can do is use the first-guess
    # astrometry which was initially passed to moggy.
    #
    # Do the calculation by getting the plate centre
    # in pixels, and converting this to SKY
    # coordinates.  NDFinforef->[2] points to an array holding coordinates
    # (lower-x, lower-y, upper-x, upper-y) for the NDF, and we take
    # the plate centre as the average of these.  
    my @platecentre;		# Centre of plate, sky coordinates, dec.deg.
    my ($pcx, $pcy);
    my @ndfbound = @{$NDFinforef->[2]};
    $pcx = ($ndfbound[0] + $ndfbound[2])/2.0;
    $pcy = ($ndfbound[1] + $ndfbound[3])/2.0;
    if (defined($bestastrometry)) {
	# Do the calculation by getting the plate centre in pixels,
	# and converting this to SKY coordinates using ATOOLS/asttran2
	my @row = ($pcx, $pcy);
	my @dat = (\@row);
	my $inndfname = twodarray2ndf (@dat, "$tempfn-coordtrans");
	my $outndfname = "$inndfname-out";
	my $status = $ATOOLS->obeyw("asttran2", "frameset=$bestastrometry incoord=$inndfname outcoord=$outndfname tosky=true degrees=true");
	($status == &Starlink::ADAM::DTASK__ACTCOMPLETE)
	  || carp "generate_astrom: error running asttran2";
	push (@tempfiles, ("$outndfname.sdf", "$inndfname.sdf"));
	my $skyd = ndf2twodarray ($outndfname);
	@platecentre = @{$skyd->[0]};
	print STDERR "Plate centre: ($pcx,$pcy)->($bestastrometry)->($platecentre[0],$platecentre[1])\n";
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
	my $platecentreref = $moggy->astconvert($pcx, $pcy, 1);
	defined($platecentreref)
	  || croak "generate_astrom: Can't convert coordinates";
	@platecentre = @$platecentreref;
	print STDERR "Plate centre: offset ($xoffset,$yoffset): ($pcx,$pcy)->($platecentre[0],$platecentre[1])\n";
    }
    
    printf ASTROMOUT "%s   %s   J2000  %s\n",
        dmshms ($platecentre[0], 1),
        dmshms ($platecentre[1], 0),
        $NDFinforef->[0];

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

    return ($astromofile, $nmatches, $matchquality);
}

# Run astrom, using the given input file.  Return an array of
# references to three-element arrays, each containing (fits-file,
# n-matches, residual), where the residual is the fifth element of the
# astrom log STAT line (it's the sum of the squares of the X and Y
# pixel residuals).  The STAT line has the form 'STAT nmatches XRMS
# YRMS RRMS residual'.
#
# Return undef on error.
sub run_astrom ($$) {
    my ($astromin, $tempfn) = @_;

    my $arep = "$tempfn-astrom.report";
    my $asum = "$tempfn-astrom.summary";
    my $alog = "$tempfn-astrom.log";
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
    my ($wcsfile,$statline);
    open (SUM, "<$alog") || do {
	print STDERR "Can't open ASTROM log file $alog\n";
	return undef;
    };
    my $l;
    while (defined ($l=<SUM>)) {
	chop $l;
	($l =~ /^FIT/) && do { $wcsfile=undef; $statline=undef; next; };

	($l =~ /^WCS *([^ ]*)/) && do { $wcsfile  = $1; next; };

	($l =~ /^STAT *(.*)/) && do { $statline = $1; next; };

	($l =~ /^ENDFIT/) && do {
	    unless (defined($wcsfile) && defined($statline)) {
		print STDERR "ASTROM logfile malformed!\n";
		next;
	    }
	    push (@tempfiles, $wcsfile);
	    my @stats = split (' ', $statline);
	    my @t = ($wcsfile, $stats[0], $stats[4]);
	    print STDERR "ASTROM logfile: @t\n";
	    push (@ret, \@t);
	    next;
	};
    }
    close (SUM);

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



1;
