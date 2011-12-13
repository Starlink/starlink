#   This file is part of autoastrom.
#
#   Copyright 2001, 2003, Council for the Central Laboratory of the Research Councils
#   except where otherwise indicated.
#
#   Portions copyright 1995, Patrick Wallace: The routine
#   decompose_transform is a direct port from the SLALIB routine
#   dcmpf.c.  All rights to this routine are reserved by Patrick
#   Wallace.
#
#   This program is part of the Starlink Software Distribution: see
#   http://www.starlink.ac.uk
#
#   autoastrom is free software (except as indicated above); you can
#   redistribute it and/or modify it under the terms of the GNU
#   General Public License as published by the Free Software
#   Foundation; either version 2 of the License, or (at your option)
#   any later version.
#
#   autoastrom is distributed in the hope that it will be useful,
#   but WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#   GNU General Public License for more details.
#
#   You should have received a copy of the GNU General Public License
#   along with autoastrom; if not, write to the Free Software
#   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
#
#   The General Public License is distributed along with this
#   program in the file LICENCE.
#
#   Author: Norman Gray <norman@astro.gla.ac.uk>
#   $Id$

# Subroutines for autoastrom.pl
#
# Broken into a separate file (a) in case I want to reuse them, and
# (b) so that I can run regression tests on them.
#
#
# $Id$

package autoastrom;
use Exporter;
use POSIX qw (sqrt atan2 sin cos fabs);
use FileHandle;

@ISA = qw(Exporter);

@EXPORT = qw( extract_objects ndf_info get_catalogue match_positions
	      generate_astrom run_astrom
	      twodarray2ndf ndf2twodarray txt2arr txt2ndf ndf2txt
	      reuse_files get_temp_files make_pseudo_fits
	      decompose_transform
	      verbosity wmessage
              check_obsdata_kwd check_kwd_list run_command_pipe);

@EXPORT_OK = (@EXPORT,
	      'dec2sex',
	      'sex2dec',
	      'ymd2je',
	      'ymd2jd',
	      'jd2je',
	      'qchi',
	      'canonicalise_ndfname',
	      'get_dates',
	      'parse_fits_date',
	      'decompose_transform',
	     );


use strict;
use Carp;

# Include the NDF modules, for the fits_read_header function
use NDF;


# Declare subroutines with prototypes
sub extract_objects ($$$$);
sub ndf_info ($$$$);
sub get_catalogue ($\%$$);
sub match_positions ($\%\%\%$);
sub match_positions_findoff ($$$$$);
sub generate_astrom ($);
sub run_astrom ($$);
sub dec2sex ($$;$);
sub sex2dec ($$);
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
sub decompose_transform ($$$$$$);
sub check_kwd_list ($$);
sub check_obsdata_kwd (\%);
sub verbosity ($);
sub wmessage ($@);
sub run_command_pipe(\@);

my $noregenerate = 0;
my @tempfiles = ();
my $verbose = 0;

# Useful values
my $d2r = 57.295779513082320876798155; # degrees to radians (quite accurately)

sub extractor_object_badness($$$$$$$) {
     my ($x2bar,$y2bar,$flux,$area,$widmin,$widmax,$densmax) = @_;

     return $widmin/$x2bar * $widmin/$y2bar * $flux/$area/$densmax;

     # The following is an alternative algorithm for badness, which
     # also weights against objects which are very broad/overexposed,
     # on the grounds that they are too smeared out to have good
     # positions.  However that's not important -- EXTRACTOR can still
     # get a good centre for them, so the position issue isn't
     # important, and they're not defects, so they _will_ appear in
     # catalogues, which is the important thing.
     #
     #     my $score = 1;
     #     my $t = $x2bar/$widmax;
     #     $score *= $widmin/$x2bar + $t*$t;
     #     $t = $y2bar/$widmax;
     #     $score *= $widmin/$y2bar + $t*$t;
     #     $score *= $flux/$area/$densmax;
     #     return $score;
}


# Extract objects from the NDF file $ndfname, using $tempdir as a
# path for temporary files, and returning a maximum of $maxobj
# objects.
#
# opts is a reference to a hash containing entries {maxobj}, the
# maximum number of objects to return (default 100); {filterdefects}
# which, if present indicates that objects which
# appear to be CCD defects should be removed (see notes below for the
# algorithm; if present it points to a hash which contains keywords).
#
# opts may also have a key {ccdcatalogue}, which contains the name of
# a file containing a catalogue of all the points on an image, in the
# format of an EXTRACTOR output catalogue.  If this is present, then
# no extraction is done, and this catalogue is instead processed as
# usual.  The catalogue must have all of the fields
# NUMBER,
# FLUX_ISO,
# X_IMAGE,
# Y_IMAGE,
# A_IMAGE,
# B_IMAGE,
# X2_IMAGE,
# Y2_IMAGE,
# ERRX2_IMAGE,
# ERRY2_IMAGE,
# ISOAREA_IMAGE,
# of which
# X2_IMAGE, Y2_IMAGE, A_IMAGE, B_IMAGE, ERRX2_IMAGE, ERRY2_IMAGE
# are not generated by default.
#
# Return a (reference to) hash containing: {filename}, filename of the
# resulting catalogue; {numobj}, the number of objects matched;
# {poserr}, std.dev. of the positions, in pixels; {objsize}, the size
# scale of the objects, in pixels; and {catalogue} an array containing
# the catalogue.
#
# The catalogue is (a reference to) an array containing (references
# to) hashes with fields {id}, {x}, {y}, {flux}, {mag} and {area}.
# The `magnitude' column isn't actually magnitude (which EXTRACTOR
# can't return); however, we wish to be able to use the `match'
# plug-in easily, which needs a flux column which is ordered like
# magnitudes: 1/flux would do, but 25-log10(flux) is more
# `magnitudish', so probably safer in the long term.
#
# This way of storing the catalogues may appear unwieldy, but (a) it
# avoids having to worry about (and remember!) the different by-column
# file formats required by the various components used below, and deal
# with the fact that different catalogues have different content (for
# example, (x,y) rather than (ra,dec)); and (b) it makes it easier to use
# alternative plugin matching routines.  It would be possible to pass
# all this information around in files (earlier versions of this code
# did this), but way too messy and confusing.
#
# On error, return undef.
sub extract_objects ($$$$) {
    my ($helpers, $ndfname, $opts, $tempdir) = @_;

    my $maxobj = $opts->{maxobj};
    defined($maxobj) || ($maxobj = 100);
    my $filterdefects = $opts->{filterdefects}; # may be undefined

    my %rethash = ();

    if (defined($opts->{fromdump})) {
	# Retrieve CCD catalogue information from a dump deposited by
	# an earlier invocation of this routine.  This is purely for
	# debugging or unit-testing (the extraction is one of the
	# slower parts of the run).

	wmessage ('info', sprintf ("Using pre-existing CCD catalogue %s",
				   $opts->{fromdump}));
	print STDERR "Reusing ", $opts->{fromdump}, "...\n" if $verbose;

	# The specified file is a re-used output of this subroutine.
	# It starts with a log of the keywords which are to be
	# returned in the hash.  Extract these, fill the hash, and
	# return.
	my $openok = open (OLDCAT, '<'.$opts->{fromdump});
	my $line;
	my @l;
	my %anonhash = ();
	if ($openok) {
	    while (defined($line = <OLDCAT>)) {
		last if ($line !~ /^\#/);
		if ($line =~ /^\#\#\s*(\S*)\s*(\S*)/) {
		    $anonhash{$1} = $2;
		}
	    }
	    # $line now has first non-comment line in it
	    my @catarr;
	    while (defined($line)) {
		@l = split (' ', $line);
		my %t;
		$t{id}   = $l[0];
		$t{x}    = $l[1];
		$t{y}    = $l[2];
		$t{flux} = $l[3];
		$t{mag}  = $l[4];
		$t{area} = $l[5];
		push (@catarr, \%t);

		$line = <OLDCAT>; # next line
	    }
	    close (OLDCAT);
	    $anonhash{catalogue} = \@catarr;

	    # return this hash
	    return \%anonhash;	# JUMP OUT
	} else {
	    wmessage ('warning',
		      sprintf ("Ooops, can't open %s.  Regenerating...",
			       $opts->{fromdump}));
	    printf STDERR ("Ooops, can't open %s.  Regenerating...\n",
			   $opts->{fromdump});
	}
    }

    my $extractorcat;
    if (defined($opts->{ccdcatalogue})) {
        $extractorcat = $opts->{ccdcatalogue};
        wmessage('info', "Using EXTRACTOR catalogue in $extractorcat");
        $rethash{provenance}
          = "From ccdcatalogue file " . $opts->{ccdcatalogue};
    } else {
        my $extractor = $helpers->{extractor};

        # We need to tell EXTRACTOR where to put the catalogue
        # output.  The custom configuration file we use sets
        # CATALOG_NAME to have the value $AUTOASTROMTEMPCATALOGUE.
        # This should have been set up already, but check it here just
        # in case.
        unless (defined $ENV{AUTOASTROMTEMPCATALOGUE}) {
            $ENV{AUTOASTROMTEMPCATALOGUE} = "$tempdir/ccdcat.extractor";
            push (@tempfiles, $ENV{AUTOASTROMTEMPCATALOGUE});
        }
        $extractorcat = $ENV{AUTOASTROMTEMPCATALOGUE};

        my $parlist = "image=$ndfname config=$ENV{AUTOASTROM_DIR}/extractor.config keywords=false";
        print STDERR "Calling EXTRACTOR: extractor $parlist\n" if $verbose;
        my $status = $extractor->obeyw ("extractor", $parlist);

        $status == &Starlink::ADAM::DTASK__ACTCOMPLETE
          || wmessage ('fatal', "Error running EXTRACTOR");

        $rethash{provenance} = "extractor $parlist";
    }

    # Sort these by column FLUX_ISO, and output at most $maxobj objects.
    open (CAT, $extractorcat) || return undef;
    print STDERR "Opened EXTRACTOR catalogue $extractorcat\n" if $verbose;
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
    printf STDERR ("First EXTRACTOR data line: <%.25s...>\n",
		   defined($line) ? $line : 'UNDEFINED')
      if $verbose;
    my $NUMBERcol = $colkey{NUMBER} - 1;
    my $FLUXcol = $colkey{FLUX_ISO} - 1;# ignores spikes, so better than FLUX_MAX
    my $Xcol = $colkey{X_IMAGE} - 1;
    my $Ycol = $colkey{Y_IMAGE} - 1;
    my $Acol = $colkey{A_IMAGE} - 1;
    my $Bcol = $colkey{B_IMAGE} - 1;
    my $X2col = $colkey{X2_IMAGE} - 1;
    my $Y2col = $colkey{Y2_IMAGE} - 1;
    my $ERRX2col = $colkey{ERRX2_IMAGE} - 1;
    my $ERRY2col = $colkey{ERRY2_IMAGE} - 1;
    my $AREAcol = $colkey{ISOAREA_IMAGE} - 1;
    (defined($NUMBERcol) && defined($FLUXcol)
     && defined($Xcol) && defined($Ycol)
     && defined($Acol) && defined($Bcol)
     && defined($ERRX2col) && defined($ERRY2col)
     && defined($AREAcol))
      || do {
          wmessage ('warning', "EXTRACTOR output $extractorcat malformed!");
          return undef;
      };

    my $nobj = 0;
    my $objrad = 0.0;
    my $objposerr = 0.0;
    my $totisoarea = 0;
    my $totisoflux = 0;
    while (defined($line)) {
	chomp $line;
	next if ($line !~ /^ *[0-9]/);
	my @l = split (' ', $line);
	# Don't use just $l[$FLUXcol] as a key, since that might not
	# be unique, and in any case hash keys are supposed to be strings.
	my $catkey = sprintf ("%015.3fi%04d", $l[$FLUXcol], $nobj);
	$cathash{$catkey} = \@l;
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

        # Sum the total flux and area, so we can crudely estimate the
        # average flux in detected objects.  This is used to set the
        # too-much-flux scale when we identify defects below.
        $totisoarea += $l[$AREAcol];
        $totisoflux += $l[$FLUXcol];

	$line = <CAT>;
    }
    close (CAT);

    # Check we found some objects in the file
    unless ($nobj > 0) {
	wmessage ('warning', 'Found zero objects in EXTRACTOR output');
	return undef;		# JUMP OUT
    }

    # Sort in reverse alphabetic order (ie, descending order of flux)
    my @sortcat = sort {$b cmp $a} keys(%cathash);

    my $defectfilterbehaviour = -1; # what do we do with defects?
                                # 0=ignore, 1=warn, 2=remove
    my $badnessthreshold = 1.0;
    foreach my $k (keys %$filterdefects) {
        # Prepare to detect or remove CCD defects.  Remember that we
        # don't have to be obsessive, here: we can afford a few
        # uncaught defects, since the match algorithms will generally
        # simply ignore these.  What we need to avoid is a CCD
        # catalogue which is dominated by bright sources which have no
        # counterpart on the sky.

        if ($k eq 'ignore') {
            if ($defectfilterbehaviour >= 0) {
                wmessage('warning', "duplicate --defects keyword $k ignored");
            } else {
                $defectfilterbehaviour = 0;
            }
        } elsif ($k eq 'warn') {
            if ($defectfilterbehaviour >= 0) {
                wmessage('warning', "duplicate --defects keyword $k ignored");
            } else {
                $defectfilterbehaviour = 1;
            }
        } elsif ($k eq 'remove') {
            if ($defectfilterbehaviour >= 0) {
                wmessage('warning', "duplicate --defects keyword $k ignored");
            } else {
                $defectfilterbehaviour = 2;
            }
        } elsif ($k eq 'badness') {
            $badnessthreshold = $filterdefects->{badness};
        } else {
            wmessage('warning', "unrecognised --defects keyword $k ignored");
        }
    }
    if ($defectfilterbehaviour < 0) {
        $defectfilterbehaviour = 1; # warn by default
    }

    # Construct an array holding references to temporary hashes each
    # containing one `row' of the catalogue of extracted objects.  In
    # this step remove any objects which fail the
    # cutoffarea/cutoffellip cuts described above, and also any for
    # which EXTRACTOR has found negative fluxes.
    my @catarr;
    my $ndefects = 0;
    my $totobjects = $#sortcat;
    my $isofluxdensity = $totisoflux/$totisoarea;
    printf STDERR ("Notional average flux density %.1f/%.1f=%.1f\n",
                   $totisoflux, $totisoarea, $isofluxdensity)
      if $verbose;

    while ($#catarr < ($maxobj-1) && $#sortcat >= 0) {
	my $l = $cathash{$sortcat[0]};
        my $badness;
        if ($defectfilterbehaviour == 0) {
            $badness = 0;
        } else {
            $badness = extractor_object_badness($l->[$X2col],
                                                $l->[$Y2col],
                                                $l->[$FLUXcol],
                                                $l->[$AREAcol],
                                                2.0,
                                                20.0,
                                                2*$isofluxdensity);
        }

        if (($l->[$FLUXcol] > 0)
            && ($defectfilterbehaviour < 2
                || $badness <= $badnessthreshold)) {

	    my %t;
	    $t{id} = $l->[$NUMBERcol];
	    $t{x} = $l->[$Xcol];
	    $t{y} = $l->[$Ycol];
	    $t{flux} = $l->[$FLUXcol];
	    $t{mag} = 25.0-log($l->[$FLUXcol])/2.30;
	    $t{area} = $l->[$AREAcol];

	    push (@catarr, \%t);
        }

        # delete negative-flux objects silently, but warn about, or log, bad ones
        if ($badness > $badnessthreshold) {
            if ($defectfilterbehaviour == 1) {
                wmessage('warning',
                         "Found likely defect at (%d,%d): badness %f, mag rank %d",
                         $l->[$Xcol],
                         $l->[$Ycol],
                         $badness,
                         $#catarr);
            } else {
                # $defectfilterbehaviour == 2
                wmessage('info', "Deleted defect at (%d,%d), badness %f",
                         $l->[$Xcol],
                         $l->[$Ycol],
                         $badness);
                $ndefects++;
            }
 	}
 	shift (@sortcat);
    }
    wmessage('warning',
             "Deleted %d/%d CCD defects", $ndefects, $totobjects)
      if ($ndefects > 0);


    $rethash{numobj} = $nobj;
    $rethash{poserr} = $objposerr/$nobj;
    $rethash{objsize} = $objrad/$nobj;


    # If requested, dump the catalogue to the specified file, for
    # later reuse by {fromdump}.
    if (defined($opts->{todump})) {
	open (DUMPCAT, '>'.$opts->{todump}) || return undef;
	print DUMPCAT "# EXTRACTOR catalogue, munged by $0\n";
	print DUMPCAT "# Input NDF: $ndfname\n";
	print DUMPCAT "# EXTRACTOR temp output: $extractorcat\n";
	print DUMPCAT "# Columns are id, x, y, flux, pseudo-mag, area\n";
	foreach my $k (keys (%rethash)) {
	    print DUMPCAT "## $k ", $rethash{$k}, "\n";
	}
	foreach my $row (@catarr) {
	    printf DUMPCAT ("%5d %12.3f %12.3f %12.1f %12.5g %12.5g\n",
			    $row->{id},
			    $row->{x},
			    $row->{y},
			    $row->{flux},
			    $row->{mag},
			    $row->{area});
	}
	close (DUMPCAT);
    }

    $rethash{catalogue} = \@catarr;

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
#    {astromtimecomment} (etc.) give further details.  The formats of
#    the entries are documented in the ASTROM manual; note that this
#    module relies on a version of ASTROM which accepts Julian epoch
#    as a valid TIME value.
#
# If there are problems getting some components of the NDF, then this
# may return with those components undefined.  On a bad error, return
# a hash containing a component {error}, which is set to an
# explanation of the problem.
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
      || do {
          $returnhash{error} = "Error running ndftrace";
          return \%returnhash;
      };

    my @dims;
    ($status, @dims) = $NDFPack->get ("ndftrace", "dims");
    ($status == $okstatus)
      || do {
          $returnhash{error} = "Error getting ndftrace/dims";
          return \%returnhash;
      };

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
	  || do {
              $returnhash{error} = "Error running wcsshow";
              return \%returnhash;
          };

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
            $value =~ s/^\s+//; # trim leading spaces
	    $fitshash{$keyword} = $value;
	}			# ignore `malformed' cards: blank, HISTORY, END
    }

    # Extract all the available date information from the NDF,
    # including from its FITS extension.
    my $ndfdates = get_dates ($indf, $helpers, \%fitshash);

    # Attempt to obtain observation for ASTROM observation records
    # ASTROM Time record. FITS values are most likely to be correct.
    if (defined($obsdata->{time})) {
        # Command-line specification of time.  We already know that
        # this is one of the three formats acceptable to ASTROM, so we
        # simply need to change separator characters to spaces.
        my $obstime = $obsdata->{time};
        $obstime =~ s/[^-0-9.]/ /;
        $returnhash{astromtime} = $obstime;
        $returnhash{astromtimecomment} = "command-line time";
    } elsif (defined($ndfdates->{fje})) {
        $returnhash{astromtime} = $ndfdates->{fje};
        $returnhash{astromtimecomment} = "FITS JE";
    } elsif (defined($ndfdates->{fst})) {
        # This is a sidereal time in sexagesimal format
        my $fst = sex2dec($ndfdates->{fst}, 'time');
        my $hrs = int($fst);
        $returnhash{astromtime} = sprintf("%f %f", $hrs, $fst-$hrs);
        $returnhash{astromtimecomment} = "FITS ST";
    } elsif (defined($ndfdates->{nje})) {
        # Epoch is a valid Time record in our custom ASTROM, but not
        # (yet?) in standard ASTROM.
        $returnhash{astromtime} = $ndfdates->{nje};
        $returnhash{astromtimecomment} = "NDF JE";
    } elsif (defined($obsdata->{ra})) {
        # We have no other information about the observation time.  In
        # this situation, the ASTROM documentation states that it is
        # legitimate to guess that the observation occurred near upper
        # culmination, so that we can set the ST to be the
        # plate-centre RA.  We don't have ready access to the
        # WCS information in the NDF, else we could use that.
        my $cmdst = sex2dec($obsdata->{ra},'time'); # NB, use RA as a _time_
        my $hrs = int($cmdst);
        $returnhash{astromtime} = sprintf("%f %f", $hrs, $cmdst-$hrs);
        $returnhash{astromtimecomment} = "command-line RA";
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
    } else {
        # Dear, dear, this is poor: we have no idea when this
        # observation took place.  Following PWD's advice, we should
        # take the epoch to be just 2000.
        # Epoch is a valid Time record in our custom ASTROM, but not
        # (yet?) in standard ASTROM.
        $returnhash{astromtime} = 2000.0;
        $returnhash{astromtimecomment} = 'Last-ditch guess of epoch 2000.0';
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
        # Just need to remove any pesky colons.
	$obsdata->{obs} =~ s/:/ /g;
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
	my $t = $fitshash{LONGITUD} * -1.0;
	my $ti = int($t);
	my $ao = sprintf ("%d %f", $ti, abs(($t-$ti)*60.0));
	$t = $fitshash{LATITUDE};
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
        $obsdate = 2000.0;
        print STDERR "ndf_info: WARNING: no date information available, epoch set to 2000.0\n"
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
# component), nje, fje (Julian epoch, decimal date -- years AD, from NDF or FITS),
# today (decimal date), fst (sidereal time of obs, from FITS file,
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
	printf STDERR ("get_dates: fje from fits_read_header{%s} = %s\n",
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
# proffered bounds of the NDF.
#
# Return a reference to a hash, with keys {catalogue}, which is a
# reference to an array of (references to) hashes, containing {id},
# {x}, {y}, {ra}, {dec} and possibly {mag}; plus key {provenance},
# which is a string reporting the origin of the catalogue.
#
# Return undef on error.
sub get_catalogue ($\%$$) {
    my $cat = shift;		# Pointer to moggy object.
    my $NDFinforef = shift;	# Reference to hash containing NDF
                                # info.
    my $maxobj = shift;		# Maximum number of objects to return.
    my $tempdir = shift;	# Directory for temporary files.

    my $mytempfile = "$tempdir/catalogue";

    # Pass the WCS information to moggy, declaring that future points
    # will be specified in the GRID domain (that domain in which the
    # first pixel is centred at coordinate (1,1)).
    $cat->astinformation ('grid', @{$NDFinforef->{wcs}});

    # Determine the bounds of the image, and make a query of all the
    # objects sitting in a box with these points at opposite
    # corners. In fact, ask for a box somewhat larger than this (say,
    # 10% in linear dimension), anticipating some misalignment in the
    # initial astrometry.  This can cause problems with the `match'
    # algorithm, since that will get terribly confused if there are
    # too many objects in the catalogue which are in the margin round
    # the CCD; it's not a problem in practice as long as we use a
    # largeish (say, 40) number of objects to match on.
    #
    # There's no need to make this margin of 10% configurable -- it
    # doesn't matter here if the projection pole is off the plate, as
    # long as the centre of _this_ plate is reasonably accurate.
    my $sizex = $NDFinforef->{dim1};
    my $sizey = $NDFinforef->{dim2};
    my $errorest = 0.1;
    my $llx = -$errorest*$sizex;
    my $lly = -$errorest*$sizey;
    my $urx = (1.0+$errorest)*$sizex;
    my $ury = (1.0+$errorest)*$sizey;
    # Or...
    # llx=1, lly=1, urx=sizex, ury=sizey

    $cat->point     ($llx, $lly);
    $cat->otherpoint($urx, $ury);
    $cat->searchtype('box');

    # Get a decent number of points
    $cat->maxrow($maxobj);

    $cat->status_ok()
      || wmessage ('fatal',
		   "Can't set catalogue parameters ("
		   . $cat->current_statusmessage() . ")");

    $cat->query()
      || wmessage ('fatal', "Can't make catalogue query ("
		   . $cat->current_statusmessage() . ")");

    if ($cat->resultnrows() == 0) {
	wmessage ('warning', "Catalogue query returned zero rows");
	return undef;		# JUMP OUT
    }

    my $sourcestring =
      sprintf ("get_catalogue: box(%.1f,%.1f)..(%.1f,%.1f) %d points",
               $llx, $lly, $urx, $ury,
               $cat->resultnrows());
    my $convref = $cat->astconvert($llx, $lly, 1);
    $sourcestring .= sprintf (": sky (%s,%s)",
                              dec2sex($convref->[0],'ra',':'),
                              dec2sex($convref->[1],'dec',':'));
    $convref = $cat->astconvert($urx, $ury, 1);
    $sourcestring .= sprintf ("..(%s,%s)",
                              dec2sex($convref->[0],'ra',':'),
                              dec2sex($convref->[1],'dec',':'));
    print STDERR "$sourcestring\n" if $verbose;

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
      || wmessage ('fatal',
		   "Can't find expected column ($xcol,$ycol,$racol,$deccol) in catalogue results\n");

    # Return an array of pointers to hashes
    my @retcat;
    my $i = 0;
    my $noob = 0;
    foreach my $row (@$resref) {
        my $x = $row->[$xcol];
        my $y = $row->[$ycol];
        if ($x < $llx || $x > $urx || $y < $lly || $y > $ury) {
            printf STDERR ("get_catalogue: (%d,%d)=(%f,%f) out of bounds\n",
                           $x, $y, $row->[$racol], $row->[$deccol])
              if $verbose;
            $noob++;
            next;
        }
	my %t = ();
	$t{id} = $i++;
	$t{x}   = $x;
	$t{y}   = $y;
	$t{ra}  = $row->[$racol]; # RA and Dec in decimal degrees
	$t{dec} = $row->[$deccol];
	$t{mag} = $row->[$magcol] if $magcol >= 0;
	push (@retcat, \%t);
    }
    if ($noob > 0 && $verbose) {
        printf STDERR
          ("Found %d/%d catalogue objects off the edge of the CCD\n",
           $noob, ($noob+$#retcat+1));
    }
    #return \@retcat;
    return { 'catalogue' => \@retcat,
             'provenance' => $sourcestring,
           };
}


# Invoke one of several routines to match the positions of the objects
# in the two input catalogues.
#
# The arguments are:
#
#    my $helpers = shift;	# Reference to hash of helper programs.
#    my $cat1 = shift;		# First position catalogue
#    my $cat2 = shift;		# Second position catalogue
#    my $matchopts = shift;	# Reference to hash of options
#				# (may include {poserr}, {objsize},
#				# {area}).  Crucially may also contain
#				# element {method}, which can be
#				# `findoff', the default, or the name
#				# of some other program loaded in a plugin.
#    my $tempfn = shift;	# Temporary filename prefix
#
# The catalogues are hash references with entries {catalogue},
# containing an array of hashes with fields {id}, {x} and {y} at
# least; and {provenance}, containing a string which reports where it
# came from.
#
# If the option {looseness} is present, then the matching algorithm
# should adjust something to make the match more likely to succeed,
# such as the position error (in the case of findoff) or the number of
# objects tested (in the case of match).  The value of the option
# should be some number which indicates the magnitude of the increased
# laxity, with 1.0 meaning no extra looseness, and 2.0, say, meaning
# twice as loose, whatever that means in a particular context.
#
# Return an array containing references to the two catalogues, in the
# same order as the corresponding input files, plus a flag (1=ok,
# 0=error) indicating whether the match succeeded or not.  The
# returned catalogues must have at least the same fields as the
# corresponding input catalogues.
#
# The returned catalogues must be the same length, and ordered so that
# entries with the same index describe the same object.  The returned
# catalogues (as a list of references) may point to the original
# hashes in the input file -- that is, they do not need to copy them.
sub match_positions ($\%\%\%$) {
    my $helpers = shift;	# Reference to hash of helper programs.
    my $cat1 = shift;		# First position catalogue
    my $cat2 = shift;		# Second position catalogue
    my $matchopts = shift;	# Reference to hash of options
                                # (may include {poserr}, {objsize},
                                # {area}).  Crucially may also contain
                                # element {method}, which can be
                                # `findoff', the default, or the name
                                # of some other program loaded in a plugin.
    my $tempfn = shift;		# Temporary filename prefix

    my $matchmethod = (defined($matchopts->{method})
		       ? $matchopts->{method}
		       : 'match');
    my $handlersub = $helpers->{'plugin-match-'.$matchmethod};
    if (defined($handlersub)) {
	wmessage ('info', "Match positions with $matchmethod");
    } else {
	wmessage ('fatal', "Plugin plugin-match-$matchmethod unknown");
    }

    return &$handlersub ($helpers, $cat1, $cat2, $matchopts, $tempfn);
}


# Invoke FINDOFF to match the positions of the objects in the two
# input catalogues.  $tempfn is a filename prefix, to make temporary
# filenames out of, not a directory.  This is invoked from
# match_positions(), and has the same interface.
#
sub match_positions_findoff ($$$$$) {
    my $helpers = shift;	# References to hash of helper programs.
    my $cat1 = shift;		# First position list
    my $cat2 = shift;		# Second position list
    my $matchopts = shift;	# Reference to hash of options
                                # (may include {poserr}, {objsize}, {area}).
    my $tempfn = shift;		# Temporary filename prefix


    my $ccdpack = $helpers->{ccdpack};
    my $myname = 'match_positions_findoff';
    # Filenames used to communicate the contents of input arrays to
    # FINDOFF, and read back the results.
    my $cat1out = "$tempfn-cat1.out";
    my $cat2out = "$tempfn-cat2.out";
    my $cat1in = "$tempfn-findoff-in-1";
    my $cat2in = "$tempfn-findoff-in-2";

    # Create two input files for FINDOFF.  FINDOFF requires that the
    # first three columns be id, x, y, but propagates any extra
    # columns from the input to the output.  Put all the information
    # in the input hashes into these extra columns (ie, redundantly
    # including id, x and y), and read them back at the end.  Thus we
    # propagate the input information into the output.  Save, in
    # cat1keys and cat2keys, the list of keys in the two input hashes,
    # so we can read the columns back in the correct order at the end.
    my @array = @{$cat1->{catalogue}};
    my @cat1keys;
    foreach my $t (keys(%{$array[0]})) {
	push (@cat1keys, $t);
    }
    open (FOFF, ">$cat1in")
      || wmessage ('fatal', "Can't open file $cat1in to write");
    my $line;
    print FOFF "# Findoff input 1.\n# Source: "
      . $cat1->{provenance}
      . "\n# columns are id, x, y, @cat1keys\n";
    foreach my $t (@array) {
	$line = sprintf ("%5d %10.2f %10.2f", $t->{id}, $t->{x}, $t->{y});
	foreach my $w (@cat1keys) {
	    $line .= sprintf (" %12.4f", $t->{$w});
	}
	print FOFF "$line\n";
    }
    close (FOFF);
    printf STDERR "match_positions_findoff input $cat1in: keys @cat1keys\n"
      if $verbose;

    @array = @{$cat2->{catalogue}};
    my @cat2keys;
    foreach my $t (keys(%{$array[0]})) {
	push (@cat2keys, $t);
    }
    open (FOFF, ">$cat2in")
      || wmessage ('fatal', "Can't open file $cat2in to write");
    print FOFF "# Findoff input 2.\n# Source: "
      . $cat2->{provenance}
      . "\n# columns are id, x, y, @cat2keys\n";
    foreach my $t (@array) {
	$line = sprintf ("%5d %10.2f %10.2f", $t->{id}, $t->{x}, $t->{y});
	foreach my $w (@cat2keys) {
	    $line .= sprintf (" %12.4f", $t->{$w});
	}
	print FOFF "$line\n";
    }
    close (FOFF);
    printf STDERR "match_positions_findoff input $cat2in: keys @cat2keys\n"
      if $verbose;

    # Write these two file names to an input file for FINDOFF
    my $findoffinfile = "$tempfn-findoff-in";
    open (FOFF, ">$findoffinfile")
      || wmessage ('fatal', "Can't open file $findoffinfile to write\n");
    print FOFF "$cat1in\n$cat2in\n";
    close (FOFF);

    # ... and a file containing the names of the output files
    my $findoffoutfile = "$tempfn-findoff-out";
    open (FOFF, ">$findoffoutfile")
      || wmessage ('fatal', "Can't open file $findoffoutfile to write\n");
    print FOFF "$cat1out\n$cat2out\n";
    close (FOFF);



    # Construct FINDOFF argument list
    # Input and output filenames
    my $findoffarg = "inlist=^$findoffinfile outlist=^$findoffoutfile";
    # ndfnames is false, so restrict and usewcs are ignored
    $findoffarg .= " ndfnames=false";
    # Log to file
    $findoffarg .= " logto=logfile logfile=$tempfn-findofflog";
    # error=1 is the default, but is a bit parsimonious.  If $matchopts
    # doesn't have an error entry, then pick 5 as a reasonable guess.
    # I'm not sure how reasonable that is, however, especially since a
    # wrongish image scale seems to throw this off badly.  If we have
    # enough points, then a figure more like 20 seems to be more
    # robust.  However, since this depends significantly on the plate
    # scale, this is just guessing.
    my $fofferr = (defined($matchopts->{poserr})
		   ? 15*$matchopts->{poserr}
		   : 20);
    if (defined($matchopts->{looseness})) {
        $fofferr *= $matchopts->{looseness};
    }
    my $foffminsep = (defined($matchopts->{objsize})
		      ? 10*$matchopts->{objsize}
		      : 5*$fofferr);
    if ($foffminsep < 5*$fofferr) {
	printf STDERR ("%s: Adjusting foffminsep %f->%f\n",
		       $myname, $foffminsep, 5*$fofferr);
	$foffminsep = 5*$fofferr;
    }
    # The default value of error is 1, but this is generally too
    # small.  It's best to specify minsep explicitly, rather than
    # relying on the dynamic default (5*error), since that can be
    # affected by the contents of the ADAM graphics database, which
    # might have been set by some intermediate by-hand run of FINDOFF.
    $findoffarg .= " error=$fofferr minsep=$foffminsep";

    # Other options
    $findoffarg .= ' fast=true failsafe=true';
    # Take maxdisp from the {area} argument, if present
    # think) from $matchopts if present.
    $findoffarg .= ' maxdisp='.sqrt($matchopts->{area})
      if defined($matchopts->{area});
    # Allow a low (ie, very tolerant) completeness.  Default is 0.5
    $findoffarg .= ' complete=0.2';
    # No need to weight by completeness (usecomp=true): since we're
    # comparing only two images, the value of usecomp is ignored.
    $findoffarg .= ' accept';	# safety net: accept default for any
                                # unspecified parameters

    if ($verbose) {
	print STDERR "$myname: Calling findoff...\n";
	foreach my $e (split(' ',$findoffarg)) {
	    print STDERR "\t$e\n";
	}
    }

    $ccdpack->contact()
      || wmessage('fatal', "Ooops, can't contact the CCDPACK monolith");
    my $status = $ccdpack->obeyw ("findoff", $findoffarg);

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
	wmessage ('fatal', "The CCDPACK monolith has unexpectedly died!");
    }

    # Check that the match worked by testing whether the output files
    # $cat1out and $cat2out have been created.
    unless (-e $cat1out && -e $cat2out) {
	wmessage ('warning',
		  "FINDOFF failed -- can't find files $cat1out and $cat2out");
	return (undef, undef, 0); # return a failure status
    }

    # Now read the output files back into new hashes.
    #
    # The FINDOFF documentation, when documenting the `position list
    # format', states that `The column one value must be an integer
    # and is used to identify positions which are the same but which
    # have different locations on different images.'  I take this to
    # mean that objects which the application has matched are given
    # the same output ID number (column 1).  In fact, these are always
    # output in numerical order, so that these output lists match line
    # by line, so it isn't necessary to explicitly match up the IDs.
    # However, this does not conform to the documentation, so we
    # should explicitly check that this is the case, and fail if it isn't.
    open (FOFF, "<$cat1out")
      || wmessage ('fatal',
		  "match_positions_findoff: Can't open file $cat1out to read");
    my @cat1resultslines = <FOFF>;
    close (FOFF);
    my @cat1results;
    my @l;
    foreach my $textline (@cat1resultslines) {
	next if ($textline =~ /^ *\#/);
	my %t;
	@l = split (' ', $textline);
	($#l == $#cat1keys + 3)
	  || wmessage ('fatal',
	       sprintf ("match_positions_findoff: assertion failed (l=%s)!=(cat1keys=%d)+3",
			$#l, $#cat1keys));
	for (my $i=0; $i<=$#cat1keys; $i++) {
	    $t{$cat1keys[$i]} = $l[$i+3];
	}
	$t{foffid} = $l[0];	# preserve the FINDOFF id number for check
	push (@cat1results, \%t);
    }
    # Same again, for catalogue 2
    open (FOFF, "<$cat2out")
      || wmessage ('fatal',
		  "match_positions_findoff: Can't open file $cat2out to read");
    my @cat2resultslines = <FOFF>;
    close (FOFF);
    my @cat2results;
    foreach my $textline (@cat2resultslines) {
	next if ($textline =~ /^ *\#/);
	my %t;
	@l = split (' ', $textline);
	($#l == $#cat2keys + 3)
	  || wmessage ('fatal',
	       sprintf ("match_positions_findoff: assertion failed (l=%s)!=(cat2keys=%d)+3",
			$#l, $#cat2keys));
	for (my $i=0; $i<=$#cat2keys; $i++) {
	    $t{$cat2keys[$i]} = $l[$i+3];
	}
	$t{foffid} = $l[0];	# preserve the FINDOFF id number for check
	push (@cat2results, \%t);
    }

    # Now perform the check, referred to above, that the corresponding
    # lines (and thus corresponding entries in the output arrays) did
    # in fact have the same ID numbers.
    ($#cat1results == $#cat2results)
      || wmessage ('fatal',
		   sprintf ("match_positions_findoff: results lists are different lengths (%d!=%d)",
			    $#cat1results, $#cat2results));
    for (my $i=0; $i<=$#cat1results; $i++) {
	($cat1results[$i]->{foffid} == $cat2results[$i]->{foffid})
	  || wmessage ('fatal',
		       sprintf ("match_positions_findoff: results lists don't match: line %d: %d!=%d",
				$i,
				$cat1results[$i]->{foffid},
				$cat2results[$i]->{foffid}));
    }

    return (\@cat1results, \@cat2results, $matchworked);
}


# Given two catalogues, generate an ASTROM input file.
#
# The input catalogues are arrays of hashes, containing at least keys
# {id}, {x} and {y} in the case of CCDin, and {id}, {ra} and {dec} in
# the case of catalogue.  The two catalogues must be the same length,
# and they are ordered, so that entries in the two catalogue arrays
# with the same index (ie $par->{CCDin}->[i] and
# $par->{catalogue}->[i]) describe the same object.
#
# We want to assemble ASTROM entries consisting of
#
#    ra      dec  0.0 0.0 J2000  * id1/id2
#    x-pos1  y-pos1
#
# for each of the pairs in the two catalogues.
#
# Return a (reference to an) anonymous hash containing keys {filename}
# (the name of the generated ASTROM input file), {nmatches} (the
# number of matches found) and {samplesd} (s.d. of residuals from the
# matching).  On any error, return the hash including a key {error},
# which contains an explanation.
#
# Return undef if we can't generate the file, for some reason.
#
# Single argument is a reference to a hash, containing keys:
#    CCDin          : EXTRACTOR output catlogue -- CCD positions
#    catalogue      : The catalogue of known positions
#    helpers        : Helper applications (ref to hash)
#    NDFinfo        : NDF information (ref to hash)
#    tempfn         : Prefix for temporary filenames
# It may additionally contain keys:
#    astrom         : Results from last ASTROM run, or undef or empty hash
#		      if none available (ie, first time) (ref to hash).
#    maxnterms	    : Maximum number of fit terms to try.
#		      6=fit posn, 7=q, 8=centre, 9=q&centre (default 6).
#
sub generate_astrom ($) {
    my $par = shift;

    my %returnhash;

    foreach my $k ('CCDin', 'catalogue', 'helpers', 'NDFinfo', 'tempfn') {
	defined($par->{$k})
	  || do {
              $returnhash{error} =
                "bad call to generate_astrom: parameter $k not specified";
              return \%returnhash;       # JUMP OUT
          };
    }

    my $tempfn = $par->{tempfn};

    # Best astrometry so far, as a FITS file, or undef if none yet.
    my $bestastrometry = (defined($par->{astrom})
			  && defined($par->{astrom}->{wcs})
			  ? $par->{astrom}->{wcs}
			  : undef);

    my @CCDarray = @{$par->{CCDin}};
    my @CATarray = @{$par->{catalogue}};
    ($#CCDarray == $#CATarray)
      || do {
          $returnhash{error} = "generate_astrom: CCDarray != CATarray";
          return \%returnhash;       # JUMP OUT
      };

    # {maxnterms} is the maximum number of fit terms to try, and
    # $nterms is the number we will try in fact.  Unless we have more
    # than 10 reference stars _and_ adequate Observation Data, $nterms
    # should be at most 6.  Don't check meteorological data -- the
    # ASTROM documentation suggests that its defaults are reasonable.
    my $nterms = (defined($par->{maxnterms}) ? $par->{maxnterms} : 6);

# PWD: users need to fit CCDs that have optical distortions, so it's
# better to switch this on and off by choice, rather than presence or
# absence of these terms. I suspect this kind of treatment isn't
# really applicable to individual CCDs (although it may be when fitting
# a whole CCD mosaic at once).
#
#    if ($nterms > 6 && ! (defined($par->{NDFinfo}->{astromtime})
#			  && defined($par->{NDFinfo}->{astromobs})
#			  # && defined($par->{NDFinfo}->{astrommet})
#			  && defined($par->{NDFinfo}->{astromcol})
#			  && ($#CCDarray >= 10))) {
    if ($nterms > 6 && ! ($#CCDarray >= 10)) {
	$nterms = 6;

        if ($#CCDarray < 0) {
            $returnhash{error} = "Ooops, input CCDarray has NO matchces";
            return \%returnhash;       # JUMP OUT
        } elsif ($#CCDarray < 10) {
	    wmessage ('warning',
		      "Too few matches ($#CCDarray).  Restricted to 6-parameter fit");
	    print STDERR "generate_astrom: Too few matches ($#CCDarray).  Restricted to 6-parameter fit\n" if $verbose;
#	} else {
#	    my $errmsg = sprintf ("Insufficient obsdata (%s, %s, %s, %s).  Can do only 6-parameter fit",
#				  (defined($par->{NDFinfo}->{astromtime})
#				   ? "time" : "notime"),
#				  (defined($par->{NDFinfo}->{astromobs})
#				   ? "obs" : "noobs"),
#				  (defined($par->{NDFinfo}->{astrommet})
#				   ? "met" : "nomet"),
#				  (defined($par->{NDFinfo}->{astromcol})
#				   ? "col" : "nocol"));
#	    wmessage ('warning', $errmsg);
#	    print STDERR "$errmsg\n" if $verbose;
	}
    }

    #+ Want to calculate $\bar x=(\sum^n x_i)/n$ and
    # $M_x=\sum^n(x_i-\bar x)^2 = \sum x_i^2 - (\sum x_i)^2/n$, and
    # the same for $(x\to y)$.  The vector $(\bar x,\bar y)$ is the
    # mean offset of the EXTRACTOR points from the catalogue points,
    # and $M=M_x+M_y$ is the statistic we examine below.
    #
    #-
    my $sumx = 0;
    my $sumy = 0;
    my $sumsq = 0;
    my ($i,$tmp);
    my $nmatches = $#CCDarray + 1;
    for ($i=0; $i<$nmatches; $i++) {
	$tmp += ($CCDarray[$i]->{x} - $CATarray[$i]->{x});
	$sumx += $tmp;
	$sumsq += $tmp*$tmp;
	$tmp = ($CCDarray[$i]->{y} - $CATarray[$i]->{y});
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



    # At this point we can indulge in a good deal of statistical
    # hi-jinks which is fun, but essentially pointless, since the
    # systematic errors dominate the random ones.
    # We do as well as we can by simply scaling $samplesd....



    my $astromofile = "$tempfn-astromin";
    open (ASTROMOUT, ">$astromofile")
      || do {
          $returnhash{error} = "Can't open file $astromofile to write";
          return \%returnhash;  # JUMP OUT
      };
    push (@tempfiles, "$astromofile");
    my $timestring = localtime();
    print ASTROMOUT "* ASTROM input file generated by...\n* $0\n* $timestring\n";
    print ASTROMOUT "* Star references are EXTRACTOR/Catalogue numbers\n";
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
	# Plate centre: These are in the GRID domain (centre of first
	# pixel at (1,1)), so to get the plate centre, we need to add
	# one to each dimension.
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
	      || do {
                  $returnhash{error} =
                    "generate_astrom: Can't convert coordinates";
                  return \%returnhash; # JUMP OUT
              };
	    @projpole = @$projpoleref;
	    printf STDERR
              ("Plate centre: offset (%.2f,%.2f)px: now (%.1f,%.1f)px at (%s,%s)\n",
               $xoffset, $yoffset, $pcx, $pcy,
               dec2sex($projpole[0],'ra',':'),
               dec2sex($projpole[1],'dec',':'))
                if $verbose;
	}
	$projpolesex[0] = dec2sex ($projpole[0], 'ra');
        $projpolesex[1] = dec2sex ($projpole[1], 'dec');
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
	printf ASTROMOUT ("%s   %s  0.0  0.0  J2000\t* %d/%d\n",
			  dec2sex($CATarray[$i]->{ra},'ra'),
			  dec2sex($CATarray[$i]->{dec},'dec'),
			  $CCDarray[$i]->{id},
			  $CATarray[$i]->{id});
	printf ASTROMOUT ("%12f   %12f\n",
			  $CCDarray[$i]->{x},
			  $CCDarray[$i]->{y});
    }

    print ASTROMOUT "END\n";
    close (ASTROMOUT);

    $returnhash{filename} = $astromofile;
    $returnhash{nmatches} = $nmatches;
    $returnhash{samplesd} = $samplesd;

    return \%returnhash;
}

# Run astrom, using the given input file.
#
# Return a reference to an array of references to anonymous hashes,
# each containing fields {nterms}, {q}, {rarad}, {decrad} (RA and Dec
# in radians), {rasex}, {decsex} (RA and Dec in sexagesimal notation),
# {wcs} (name of FITS-WCS file), {nstars}, {prms} (RMS residual in
# pixels), {STATUS} (true if the fit was OK, false otherwise).
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
    my @astromargs = ("$ENV{ASTROM_DIR}/astrom.x",
                   "input=$astromin",
                   "report=$arep",
                   "summary=$asum",
                   "fits=$tempfn-astromout-wcs",
                   "log=$alog");
    if ($verbose) {
        printf STDERR ("Starting ASTROM:");
        foreach my $w (@astromargs) {
            printf STDERR ("\t%s \\\n", $w);
        }
    }
    my $astromexit = system(@astromargs);

    if ($astromexit != 0) {
        print STDERR "Failed calling ASTROM\n";
        return undef;
    }
    print STDERR "...OK\n" if $verbose;

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

	($l =~ /^RESULT +(\S+)\s+(\S+)/) && do {
	    $results{$1} = $2;
	    next;
	};

	($l =~ /^STATUS +(\S+)/) && do {
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

    ($#ret > 0) || print STDERR "run_astrom: no return values! Either astrom failed, or the log file was incomplete\n";

    return \@ret;
}



# Convert decimal degrees/times to sexagesimal.
#
# Args:
#    $val = angle in decimal degrees
#
#    $type = 'ra', this is an RA in degrees, return HMS; 'dec', this is a
#    Dec, return DMS; 'time', this is a time in hours, return HMS.
#
#    $sep = if present, use this as separator (default ' ')
#
# Return a string with two-digit (leading-zero) minutes and seconds,
# and three-digit fraction.
#
# Return undef on range errors.
sub dec2sex ($$;$) {
    my ($val, $type, $sep) = @_;
    my ($dh, $min, $sec, $mas, $sign);
    # Check we haven't been given a sexagesimal value by mistake.
    if ($val !~ /^ *[-+]?[0-9]*(\.[0-9]*)? *$/) {
	print STDERR "dec2sex: decimal $val malformed\n";
	return undef;
    }
    defined($sep) || ($sep = ' ');
    # Add 0.5m(a)s to the value, and do rounding explicitly.
    # Otherwise values such as 40 degrees turns into 2.66..667 hours,
    # 2:39:59.999..., which is rounded to 2:39:60 rather than
    # 2:40:00. 1m(a)s = 0.5/3600/1000 = 1.3888e-7;
    if ($type eq 'dec') {
        # $sign holds sign of $val, as +1 or -1; $val holds absolute value of $val
	if ($val >= 0) {
	    $sign = 1;
	} else {
	    $sign = -1;
	    $val = -$val;
	}
	if ($val > 90) {
	    print STDERR "dec2sex: decimal Dec $val out of range\n";
	    return undef;
	}
	# val=90.0000001388 will round to 90
        $val += 0.5/3.6e6;
	#$val += 0.0000001388;
    } elsif ($type eq 'ra' || $type eq 'time') {
	$val /= 15 if $type eq 'ra'; # convert to hours
	$val += 0.5/3.6e6;      # add 0.5ms = 7.5mas
        # normalise
	while ($val < 0) { $val += 24; }
	while ($val >= 24) { $val -= 24; }
	$sign = 1;
    } else {
        print STDERR "dec2sex: unrecognised type $type\n";
        return undef;
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
# into decimal degrees, as a double.
#
# Args:
#
#    $val: the value to be converted, as a string containing a
#    colon-separated sexagesimal angle.  The value must have all of
#    d/h, minutes and seconds, separated by colons, with no internal
#    spaces, though there may be leading and trailing whitespace.
#
#    $type: 'ra': the first argument is an RA in HMS; 'dec': the
#    argument is a Dec in DMS; 'time' the argument is a time in HMS.
#
# If angle argument is undef, silently return undef.
#
# Return the value as a decimal number of degrees (in the case of 'ra'
# or 'dec') or hours (in the case of 'time'), or undef on formatting/range
# errors.
sub sex2dec ($$) {
    my ($val, $type) = @_;

    defined($val) || return undef;

    my ($sign,$dh,$min,$sec);
    if (($sign,$dh,$min,$sec) = ($val =~ /^ *([-+]?)([0-9]+):([0-9]+):([0-9]+(\.[0-9]+)?) *$/)) {
	my $rval = $dh + $min/60.0 + $sec/3600.0;
	if ($type eq 'dec') {
            if ($sign eq '-' && $rval != 0.0) {
                $rval = -$rval;
            }
	    if ($rval < -90.0 || $rval > +90.0) {
		print STDERR "sex2dec: sexagesimal Dec $val --> $rval out of range\n";
		return undef;
	    }
	} elsif ($type eq 'ra') {
            if ($sign eq '-') {
                print STDERR "sex2dec: RA $val --> must be positive\n";
                return undef;
            }
	    $rval *= 15;	# Hours --> degrees
	    if ($rval == 360.0) { $rval = 0.0; } # accept 360, but fix
	    if ($rval < 0.0 || $rval >= 360.0) {
		print STDERR "sex2dec: sexagesimal RA $val --> $rval out of range\n";
		return undef;
	    }
	} elsif ($type eq 'time') {
            if ($sign eq '-') {
                print STDERR "sex2dec: Time $val --> must be positive\n";
                return undef;
            }
	    if ($rval == 24.0) { $rval = 0.0; } # accept 24, but fix
	    if ($rval < 0.0 || $rval >= 24.0) {
		print STDERR "sex2dec: sexagesimal time $val --> $rval out of range\n";
		return undef;
	    }
    } else {
        print STDERR "sex2dec: unrecognised type $type\n";
        return undef;
	}
	# All OK
	return $rval;
    } else {
	print STDERR
          "sex2dec: sexagesimal angle/time $val malformed, must be d/h:m:s\n";
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
# Formulas'.  Return negative on error.
sub ymd2jd ($$$) {
    # Use the standard formula to convert Gregorian dates to Julian
    # Day numbers
    use integer;
    my ($year, $month, $day) = @_;
    # Year is (1000..3000), month is in (1..12), day in (1..31).  The
    # restriction on year is not because of any limitation on the
    # validity of the formula, but to guard against silly parameters
    # (eg, 2-digit dates).
    my $err;
    ($year > 1000 && $year < 3000)
      || do { $err = "ymd2jd: years in 1000..3000 only"; };
    ($month >= 1 && $month <= 12)
      || do { $err = "ymd2jd: month $month out of range"; };
    ($day >= 1 && $day <= 31)
      || do { $err = "ymd2jd: day $day out of range"; };

    if (defined $err) {
        wmessage('warning',
                 "Invalid input to ymd2jd: $err.  Results probably nonsense");
        return -1;
    } else {
        return $day - 32075 + 1461*($year+4800+($month-14)/12)/4
          +367*($month-2-($month-14)/12*12)/12
            -3*(($year+4900+($month-14)/12)/100)/4;
    }
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
}

# Convert a FITS date into a Julian epoch.  After the 1997-11-10
# amendment documented in
# <http://www.cv.nrao.edu/fits/documents/standards/year2000.txt>, FITS
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
# coordinates in FITS'; A&A 395, 1061--1075 (2002); online at
# <http://www.cv.nrao.edu/fits/documents/wcs/wcs.html>).
#
# Return a reference to an array holding a sequence of FITS cards, or
# undef on error.  The routine should produce a valid sequence of FITS
# cards, so the SIMPLE, BITPIX and NAXIS keywords are included.
sub make_pseudo_fits (\%\%) {
    my ($kv,$NDFinfo) = @_;

    (defined($kv->{ra}) && defined($kv->{dec})) || do {
	print STDERR "pseudo.fits: WCS info must have at least ra and dec\n";
	return undef;
    };

    my $radeg  = ($kv->{ra}=~/:/  ? sex2dec($kv->{ra},'ra')  : $kv->{ra});
    my $decdeg = ($kv->{dec}=~/:/ ? sex2dec($kv->{dec},'dec') : $kv->{dec});

    # Did the sex2dec conversion work?
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
    foreach my $testkw (sort(keys(%$kv))) {
	$logstring .= ' '.$testkw.'='.$kv->{$testkw};
    }
    print STDERR "logstring=$logstring\n";
    push (@pfitsarray, sprintf ("%-8s %.71s", 'COMMENT', $logstring));
    # Coordinate value at reference point
    push (@pfitsarray, sprintf ("%-8s= %20f / RA    %s",
				'CRVAL1', $radeg,dec2sex($radeg,'ra')));
    push (@pfitsarray, sprintf ("%-8s= %20f / Dec   %s",
				'CRVAL2', $decdeg, dec2sex($decdeg,'dec')));
    # units (default degrees)
    push (@pfitsarray, sprintf ("%-8s= '%-10s'", 'CUNIT1', 'deg'));
    push (@pfitsarray, sprintf ("%-8s= '%-10s'", 'CUNIT2', 'deg'));
    # Pixel location of reference point
    push (@pfitsarray, sprintf ("%-8s= %20f", 'CRPIX1',
				($NDFinfo->{dim1}+1)/2));
    push (@pfitsarray, sprintf ("%-8s= %20f", 'CRPIX2',
				($NDFinfo->{dim2}+1)/2));

    my $pa = (defined($kv->{angle}) ? $kv->{angle} : 0);
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

# decompose_transform: decompose a linear fit into zero shifts,
# scales, nonperpendicularity and orientation.
#
# Given a model:
#
#    x_2 = a + b x_1 + c y_1
#    y_2 = d + e x_1 + f y_1
#
# this routine decomposes it as detailed below.
#
# Arguments: (a, b, c, d, e, f)
#
# Return value: an array containing (xz, yz, xs, ys, perp, orient)
# (see below).  The only difference from the SLALIB original is that
# the perp and orient fields are returned in degrees rather than radians.
#
# This is a direct port of SLALIB's routine dcmpf.c.  It is Copyright
# 1995, Patrick Wallace.  All rights reserved.
sub decompose_transform ($$$$$$) {
    #
    #  - - - - - - - - -
    #   s l a D c m p f
    #  - - - - - - - - -
    #
    #  Decompose an [x,y] linear fit into its constituent parameters:
    #  zero points, scales, nonperpendicularity and orientation.
    #
    #  Given:
    #     coeffs    double[6]     transformation coefficients (see note)
    #
    #  Returned:
    #     *xz       double        x zero point
    #     *yz       double        y zero point
    #     *xs       double        x scale
    #     *ys       double        y scale
    #     *perp     double        nonperpendicularity (radians)
    #     *orient   double        orientation (radians)
    #
    #  The model relates two sets of [x,y] coordinates as follows.
    #  Naming the elements of coeffs:
    #
    #     coeffs[0] = a
    #     coeffs[1] = b
    #     coeffs[2] = c
    #     coeffs[3] = d
    #     coeffs[4] = e
    #     coeffs[5] = f
    #
    #  The model transforms coordinates [x1,y1] into coordinates
    #  [x2,y2] as follows:
    #
    #     x2 = a + b*x1 + c*y1
    #     y2 = d + e*x1 + f*y1
    #
    #  The transformation can be decomposed into four steps:
    #
    #     1)  Zero points:
    #
    #             x' = xz + x1
    #             y' = yz + y1
    #
    #     2)  Scales:
    #
    #             x'' = xs*x'
    #             y'' = ys*y'
    #
    #     3)  Nonperpendicularity:
    #
    #             x''' = cos(perp/2)*x'' + sin(perp/2)*y''
    #             y''' = sin(perp/2)*x'' + cos(perp/2)*y''
    #
    #     4)  Orientation:
    #
    #             x2 = cos(orient)*x''' + sin(orient)*y'''
    #             y2 =-sin(orient)*y''' + cos(orient)*y'''
    #
    #  See also slaFitxy, slaPxy, slaInvf, slaXy2xy
    #
    #  Last revision:   22 September 1995
    #
    #  Copyright P.T.Wallace.  All rights reserved.

    # Arguments
    my ($a,$b,$c,$d,$e,$f) = @_;


    my ($rb2e2, $rc2f2, $xsc, $ysc, $p, $ws, $wc, $or);
    my ($hp, $shp, $chp, $sor, $cor, $det, $x0, $y0);

    my $pi = 2.0 * POSIX::asin(1.0);

    # Scales
    $rb2e2 = POSIX::sqrt ( $b * $b + $e * $e );
    $rc2f2 = POSIX::sqrt ( $c * $c + $f * $f );
    if ( ( $b * $f - $c * $e ) >= 0.0 ) {
	$xsc = $rb2e2;
    } else {
	$b = -$b;
	$e = -$e;
	$xsc = -$rb2e2;
    }
    $ysc = $rc2f2;

    # Non-perpendicularity
    $p = ( ( $c != 0.0 || $f != 0.0 ) ? POSIX::atan2 ( $c, $f ) : 0.0 ) +
         ( ( $e != 0.0 || $b != 0.0 ) ? POSIX::atan2 ( $e, $b ) : 0.0 );
    # ...and ensure it's in [-Pi..+Pi]
    while ($p < -$pi) { $p += 2*$pi; }
    while ($p > +$pi) { $p -= 2*$pi; }

    # Orientation
    $ws = ( $c * $rb2e2 ) - ( $e * $rc2f2 );
    $wc = ( $b * $rc2f2 ) + ( $f * $rb2e2 );
    $or = ( $ws != 0.0 || $wc != 0.0 ) ? POSIX::atan2 ( $ws, $wc ) : 0.0;

    # Zero corrections
    $hp = $p / 2.0;
    $shp = POSIX::sin ( $hp );
    $chp = POSIX::cos ( $hp );
    $sor = POSIX::sin ( $or );
    $cor = POSIX::cos ( $or );
    $det = $xsc * $ysc * ( $chp + $shp ) * ( $chp - $shp );
    if ( POSIX::fabs ( $det ) > 0.0 ) {
	$x0 = $ysc * ( $a * ( ( $chp * $cor ) - ( $shp * $sor ) )
		       - $d * ( ( $chp * $sor ) + ( $shp * $cor ) ) ) / $det;
	$y0 = $xsc * ( $a * ( ( $chp * $sor ) - ( $shp * $cor ) )
		       + $d * ( ( $chp * $cor ) + ( $shp * $sor ) ) ) / $det;
    } else {
	$x0 = 0.0;
	$y0 = 0.0;
    }

    # Results
    return ($x0, $y0, $xsc, $ysc, $p*$d2r, $or*$d2r);
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
	return "$k=".$t->{$k} unless $t->{$k} =~ m{$pattern};
    }
    return undef;
}

sub check_obsdata_kwd (\%) {
    my $t = shift;
    my %goodkws = (
		   'ra' => '\s*[+-]?\d{1,3}((:\d{1,2}){0,2}(\.\d+)?)\s*',
		   'dec' => '\s*[+-]?\d{1,2}((:\d{1,2}){0,2}(\.\d+)?)\s*',
		   'scale' => '\s*\d+(\.\d+)?\s*',
		   'invert' => '\s*[01]\s*',
		   'angle' => '\s*[+-]?\d+(\.\d+)?\s*',
		   'source' => '(?i)\s*(AST|FITS|USER)(\s*:\s*(AST|FITS|USER))*\s*',
		   'time' => '\s*(\d+\.\d+|\d{4}([: ]+\d{1,2}){4}(\.\d+)?|\d{1,2}[: ]+\d{1,2}(\.\d+)?)\s*',
		   'obs' => '\s*(\w+|([: ]*[+-]?\d+[: ]+\d+(\.\d*)?){2}([: ]*\d*(\.\d*)?)?|\w+\.\w+)\s*',
		   'met' => '\s*\d+(\.\d+)?([: ]*\d+(\.\d+)?)\s*',
		   'col' => '\s*\d+\s*'
		  );

    return check_kwd_list ($t, \%goodkws);
}

# Runs the given command in a pipe, and collects the command's stdout
# into an array of strings with any trailing whitespace discarded, and
# returns a reference to this array.  Returns undef on any error.
sub run_command_pipe (\@) {
    my $argref = shift;
    #my @cmdargs = @$argref;
    my $rval;

    my $arg = join(' ',@$argref) . '|';
    my $pid = open (P, $arg);
    if (! defined($pid)) {
        wmessage('warning', "Couldn't fork to run $arg");
        $rval = undef;
    } else {
        my @lines;
        while (<P>) {
            s/\s*$//;
            printf STDERR "%s: <%s>\n", $argref->[0], $_;
            push(@lines, $_);
        }
        close(P);
        # Ignore the termination status -- it doesn't seem to reliably correlate
        # with $#lines>=0
        my $stat = $?;
        if ($verbose) {
            if ($stat != 0) {
                printf STDERR ("Closing %s pipe failed: %d\n",
                               $argref->[0], $stat);
            }
            if ($#lines < 0) {
                printf STDERR ("%s produced no output\n",
                               $argref->[0]);
            }
        }

        if ($#lines < 0) {
            $rval = undef;
        } else {
            $rval = \@lines;
        }
    }

    # The alternative way to do this is to do
    #
    #    my $pid = open(P,'-|');
    #
    # then exec(@cmdargs) in the child.  That does NOT work, however,
    # as the first time we read from the pipe in the parent, we
    # usually, but not always, read EOF (while(<P>) returns false.  I
    # have no idea why, and spent ages trying to work how how Perl
    # could be stuffing this up, but finally realised that there was
    # nothing particular to be gained by doing this, other than a warm
    # glow of system programming....

    return $rval;
}

# Wmessage writes a message on the stdout.  $1 is one of `info',
# `warning', `fatal', with unrecognised strings being fatal (checks
# for errors).   $2 is the message.  In the case of `fatal'
# messages, the routine then croaks.
sub wmessage ($@) {
    my $type = uc(shift());
    my $fmt = shift;
    my @prefixes = ('--I ', '--W ', '--E ');
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
	$prefix = "$prefixes[2] XXXX WHAT is msg ($type)?    ";
	$croak = 1;
    }

    my $msg = ($#_ >= 0 ? sprintf($fmt,@_) : $fmt);

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
