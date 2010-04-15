#!/local/bin/perl

# Calculate the error in the clock between SCUVAX and MWTTEL.
# Do this by comparing the Az and El recorded in the header
# with the time stored in the header.

# POD Documentation is at the end of the file
# -v option can be used to turn on verbose messages

# Loops over all the supplied files

use strict;
use Astro::SLA qw/ D15B2P DR2S DR2H DS2R slaDh2e slaDafin slaMap slaCldj
  slaFk45z slaGaleq slaDranrm D2PI DPI
  slaDr2tf slaDr2af slaDrange/;
use Math::Trig qw/ deg2rad rad2deg /;  # deg2rad, rad2deg
use NDF;         # fits_read_header
use Getopt::Long;
use Pod::Usage;
use File::Temp;

# Wrapper for PI
use constant HALFDAY => DPI;

use vars qw/ $DEBUG $v/;

# Option handling
my $man = 0;
my $verbose = 0;
my $help = 0;

GetOptions('help|?' => \$help, man => \$man, verbose => \$verbose,
	  ) or pod2usage(2);

pod2usage(1) if $help;
pod2usage(-verbose => 2) if $man;


# Simple options handling via -s
$DEBUG = ( $verbose ? 1 : 0 );


# Loop over input files
foreach my $file (@ARGV) {

    my $temp = 0; # are we using a temp file
    if ($file =~ /\.gz/) {
	# GUNZIP it to /tmp - very inefficient way of reading the fits header
	my $output = tmpnam . '.sdf';
	my $stat = system "gzip -dc $file > $output";
        die "Error in gzip: $!" if $stat != 0;
	$file = $output;
	$temp = 1;
    }

    # Read the fits header from the supplied filenames
    my ($href, $status) = fits_read_header( $file );
    next if $status != &NDF::SAI__OK;

    my ($mjd, $err, $utdate, $run, $iter) = calc_clock_err($href, $file);

    if (defined $mjd) {

	printf "%.3f %.2f %s %d %d iterations\n",$mjd,$err,$utdate,$run,$iter;

    }

    # remove temp file if required
    unlink $file if $temp;
}


# unnnecessary
exit;

# subroutines

sub calc_clock_err {
    my $href = shift;
    my $file = shift; # for LST_STRT array

    # Don't bother if we have a skydip
    return () if $href->{MODE} eq 'SKYDIP';

    # Calculate the hour angle
    slaDh2e(deg2rad($href->{STRT_AZD}),
	    deg2rad($href->{STRT_ELD}),
	    deg2rad($href->{'LAT-OBS'}),
	    my $ha, my $dec);

    print "Hour angle : $ha [radians] ", $ha * DR2H, " [hours]\n"
	if $DEBUG;
    print "Declination: $dec [radians] ", rad2string($dec),"\n"
	if $DEBUG;

    # Read the LST from the header and the LST from the LST_STRT
    # array and determine the error in the UTSTART
    my $lst_hdr = string2rad( $href->{STSTART}, 1 );

    # Get the LST information from the LST_STRT array
    my ($status, @lst) = get_lst( $file );
    return () if $status != &NDF::SAI__OK;

    # Check that LST array is populated
    return () if ($href->{STATE} =~ /Abort/i && $lst[0] == 0.0);

    # The error in UTSTART is therefore
    # Need to watch out for 23h59 subtracting from 00h01
    my $UTerr = slaDrange($lst[0] - $lst_hdr);
    my $UTerr_sec = DR2S * $UTerr;
    print "Start up time in sec : ", $UTerr_sec," [$UTerr]\n" if $DEBUG;

    # The start up time can not be negative since the LST_STRT
    # array is always written after ths STSTART header
    if ($UTerr_sec < 0) {
      print "*****WARNING***** Startup time can not be negative *******\n";
      print "*****WARNING***** Setting error to 0.0             *******\n";
      $UTerr_sec = 0;
    }


    # We iterate over the calculation until the difference between
    # the calculated clock error (using the incorrect headers) and
    # and when using the correction calculated previously roun the loop.
    # This has to be done since the apparent RA/Dec changes quite a lot
    # if you introduce a 5 minute error in MJD.
    # Stop looping if we have to go round more than 10 times

    my $mjd;  # need this outside of while loop
    my $tries = 0;
    my $current  = 0.0;
    my $previous = -999; # infinity since we have no previous yet
    while (abs($current - $previous) > 0.05 ) {
	$previous = $current;

	# Calculate the apparent ra/dec of the tracking centre at LST
	# stored in header. Technically we need to calculate it for the actual
	# time.... so correct using the current best guess at the real
	# time correction.
	# Returns ra/dec in radians
	my ($ra_app, $dec_app) = calc_apparent( $href, $UTerr_sec + $current );
	print "Apparent RA/Dec = ", rad2string($ra_app,1), ",",
	    rad2string($dec_app,0)," [$ra_app, $dec_app]\n"
		if $DEBUG;
	return () unless defined $ra_app;

	# Calculate the LST from the hour angle and the apparent ra/dec
	# normalising it to the range 0 to 2PI)
	my $lst_azel = slaDranrm( $ra_app + $ha );

	print "LST stored in LST_STRT : ", rad2string($lst[0], 1),
	    " [$lst[0]]\n" if $DEBUG;

	print "LST derived from Az/EL : ", rad2string($lst_azel,1),
	    " [$lst_azel]\n" if $DEBUG;
	print "LST stored in header   : ", rad2string($lst_hdr, 1),
	    " [$lst_hdr]\n" if $DEBUG;


	# Calculate the difference between the azel time and the
	# time read from LST_STRT
	# Use the sense that the resulting error must be
	# added to @lst in order to obtain the correct time
	my $err = slaDrange($lst_azel - $lst[0]);

	# Convert the error to seconds
	my $err_sec = $err * DR2S;
	$mjd     = calc_mjd($href->{UTDATE},$href->{UTSTART},
			    $UTerr_sec + $err_sec);
	printf "%.3f %.2f %s %d\n",$mjd,$err_sec,$href->{UTDATE}, $href->{RUN}
	    if $DEBUG;

	$current = $err_sec;
	$tries++;
	die "Could not converge on solution after $tries iterations"
	    if $tries > 10;

	# Only need to do this correction if we are a moving source
	last unless $href->{CENT_CRD} =~ /PL/;

    }

    $tries--; # The last iteration doesnt count

    return ($mjd, $current, $href->{UTDATE}, $href->{RUN}, $tries);
}


# Subtract two LST times (in radians) but assuming that the difference
# must be less than half a day [ie the difference between 23h59 and
# 00h01 is 2 minutes and not almost 24 hours

# Conceptually for
#    $diff = sub_lst( $t1, $t2);
# the answer is
#    $diff = $t1 - $t2

sub sub_lst ($$) {
    my ($t1, $t2) = @_;

    my $diff = slaDrange( $t1 - $t2 );

    if (abs($diff) > HALFDAY) {
	print "In sub with large diff: $diff\n" if $DEBUG;
	if ($diff > 0) {
	    # t1 is much larger than t2 therefore add 2PI to t2
	    # or subtract 2PI from diff
	    $diff -= D2PI;
	    print "Subtracted 2PI diff now: $diff\n"
		if $DEBUG;
	} else {
	    # t2 is much larger than t1. Add 2PI to diff
	    $diff += D2PI;
	    print "Added 2PI diff now: $diff\n"
		if $DEBUG;
	}
    }
    return $diff;
}


# Convert string of form  hh:mm:ss.s or dd:mm:ss.s
# to radians. Returns undef on error.
# Second argument indicates whether it is a time (1) or an angle (0)
#  $rad = string2rad("18:15:22.0", 1);

sub string2rad ($$) {
    my $ra = shift;
    my $istime = shift;

    # Replace : with space
    $ra =~ s/:/ /g;

    # Convert the RA from the header to radians
    my $pos = 1;
    slaDafin($ra, $pos, my $reslt, my $jf);
    $reslt *= 15.0 if $istime;

    return ( $jf == 0 ?  $reslt : undef);
}


# Convert radians to a string format
#   $string = rad2string($rad, $istime);
# The second argument should be true if we want h:m:s
# false if we want d:m:s

sub rad2string ($$) {
    my $rad = shift;
    my $istime = shift;

    my (@ihmsf, $sign);
    if ($istime) {
	slaDr2tf(2, $rad, $sign, @ihmsf);
    } else {
	slaDr2af(2, $rad, $sign, @ihmsf);
    }
    # Format the string
    my $string = sprintf("%d %d %d.%02d", @ihmsf);

    # prepend negative if required
    $string = $sign . $string if $sign eq '-';

    return $string;
}


# Calculate modified julian date from SCUBA UTSTART and UTDATE
# returns undef on error. An optional offset in seconds can be
# supplied to this routine (usually zero) indicating an
# error in the UTSTART string [with SCUBA the times are written
# before we are on source so there is a discrepancy]
# The offset is added to the MJD and can be negative or positive.

sub calc_mjd ($$$) {
    # Split the date string into parts
    my @date = split(/:/, $_[0]);

    # Now convert the time to radians so we can determine
    # the fraction of day
    my $pos = 1;
    my $uttime = $_[1];
    $uttime =~ s/:/ /g;
    slaDafin($uttime, $pos, my $reslt, my $jf);
    return undef if $jf != 0;

    # Convert the result to a time in radians
    $reslt *= 15.0;

    # Add the error (in seconds converted to radians)
    $reslt += DS2R * $_[2];

    # And convert the result to fraction of day
    my $frac = $reslt / D2PI;

    # Calculate the MJD from the date
    slaCldj( $date[0], $date[1], $date[2], my $mjd, my $status);
    return undef if $status != 0;

    # Add on the fraction
    $mjd += $frac;
    return $mjd;
}

# Calculate the apparent ra/dec from the SCUBA header information.
# Currently can only handle RJ, RB, GA and PLANET, RD
# Expects a hash with SCUBA headers. Calculates it for the supplied
# UTSTART, UTDATE  values (which are used to derive MJD)
# Returns apparent ra/dec or an empty list.

# The second argument is the error in the times found in the headers
# in seconds. It is required for the MJD calculation.

sub calc_apparent ($$) {
    my $hdr = shift;
    my $uterr = shift;

    # Read all the header info we require
    my $coord_type = $hdr->{CENT_CRD};

    # Copy some values to save typing later
    my $long;
    if ($coord_type =~ /^[REP]/) { # RA,RJ,EQ,PL
	# We have a right ascension
	$long = string2rad($hdr->{LONG},1);
    } else {
	# We have degrees
	$long = string2rad($hdr->{LONG},0);
    }
    my $long2= string2rad($hdr->{LONG2},1);
    my $lat  = string2rad($hdr->{LAT},0);
    my $lat2 = string2rad($hdr->{LAT2},0);
    my $mjd1 = $hdr->{MJD1};
    my $mjd2 = $hdr->{MJD2};

    # Calculate the MJD
    my $mjd = calc_mjd( $hdr->{UTDATE}, $hdr->{UTSTART}, $uterr);
    if ($DEBUG) {
	print "MJD = $mjd Cord = $coord_type\n";
	print "String inputs: ", $hdr->{LONG} , ",", $hdr->{LAT},"\n";
	print "Input coords: $long, $lat\n";
    }

    # First convert from the input frame to RJ
    if ($coord_type eq 'RB' || $coord_type eq 'RJ' || $coord_type eq 'GA') {
	# Initialise J2000 RA/DEc variables
	my ($raj, $decj) = ($long, $lat);
	if ($coord_type eq 'RB') {
	    # RB -> RJ
	    slaFk45z( $raj, $decj, 1950.0, $raj, $decj);
	} elsif ($coord_type eq 'GA') {
	    # GA -> RJ
	    slaGaleq( $raj, $decj, $raj, $decj);
	}

	# Now convert to apparent
	slaMap( $raj, $decj, 0.0, 0.0, 0.0, 0.0, 2000.0, $mjd,
		my $ra_app, my $dec_app);

	return ($ra_app, $dec_app);

    } elsif ($coord_type eq 'RD') {

	# Do nothing
	return ($long, $lat);

    } elsif ($coord_type eq 'PLANET') {
	# Need to interpolate to current MJD
	if ($mjd1 == $mjd2) {
	    # ASsume a constant
	    return ($long, $lat);
	} else {
	    # interpolate
	    my $ra_app = $long + ( $long2 - $long ) * ( $mjd - $mjd1 ) /
		($mjd2 - $mjd1);
	    my $dec_app =$lat  + ( $lat2  - $lat  ) * ( $mjd - $mjd1 ) /
		($mjd2 - $mjd1);
	    return ($ra_app, $dec_app);
	}

    }

    # Return empty list on error
    return ();
}


# Retrieve the lst array from an NDF
#  ($status, @lst) = get_lst( $file );
# Array is returned in radians

sub get_lst ($) {

    my $status = &NDF::SAI__OK;

    # Read in filename and strip .sdf
    my $filename = shift;
    $filename =~ s/\.sdf//;

    ndf_begin;
    ndf_find(&NDF::DAT__ROOT, $filename, my $indf, $status);
    ndf_xloc($indf, 'SCUCD', 'READ', my $xloc, $status);

    my $comp = 'LST_STRT';
    cmp_size($xloc, $comp, my $size, $status);
    cmp_getvd($xloc, $comp, $size, my @lst, my $el, $status);

    dat_annul($xloc,$status);
    ndf_annul($indf, $status);

    ndf_end($status);

    return ($status,@lst);

}

__END__

=head1 NAME

scuvax_clkerr - Determine error in SCUVAX clock from SCUBA headers

=head1 SYNOPSIS

 scuvax_clkerr <file1> <file2> ... <filen>

 scuvax_clkerr -verbose <file>
 scuvax_clkerr -help

=head1 DESCRIPTION

This program attempts to calculate the clock error found in a SCUBA
data file. Multiple files can be analyzed and the results are written
to standard output.

The default output is a line to STDOUT containing the
modified julian date, the clock error, the UT date and the run number:

 51492.80 -46 1999:11:10 89

In verbose mode much more information is provided:


 Hour angle : 0.26333252840521 [radians] 1.00585616574183 [hours]
 Start up time in sec : 29.6434760028357
 MJD = 51492.7968015005 Cord = RJ
 String inputs: 10:56:59,-03:37:35
 Input coords: 2.86663057434852, -0.0632924260688499
 Apparent RA/Dec = 10 56 57.14,-3 37 21.37
 LST stored in LST_STRT : 11 56 32.17 [3.1264788702126]
 LST derived from Az/EL : 11 57 18.22 [3.12982748836973]
 LST stored in header   : 11 56 2.53 [3.12432313580433]
 51492.80 -46 1999:11:10 89

The reported modified julian date has been corrected for this
clock error (ie it is the actual MJD of the observation rather
than the MJD stored in the header).


=head1 OPTIONS

The following options are recognised:

=over 4

=item B<-help>

Print a brief help message and exits.

=item B<-man>

Prints the manual page and exits.

=item B<-verbose>

Prints detailed information concerning the calculation of the error.

=back

=head1 METHOD

This program uses the fact that the azimuth and elevation stored
in the SCUBA data headers (C<STRT_ELD> and C<STRT_AZD> are read
from the telescope at the same time that the (incorrect) start
time for the first switch is read (and stored in the C<LST_STRT>
array). The SCUBA headers therefore allow the actual LST to be
calculated from the hour angle and the RA/Dec the telescope was
known to be observing.

The C<STSTART> header item (or the related C<UTSTART> and C<HSTSTART>)
can not be used to determine the timing error since these are written
when the observation begins rather than when data acquisition begins.
The difference between these two times is at least 25 seconds (the
time it takes to set up SCUBA for the observation) but can be much
longer if the telescope has a long slew to get on source. This
time difference is calculated by the program and displayed if
verbose mode is used.

The accuracy of the calculation is approximately E<plusmn>10 seconds.
This is probably due to minor differences in the time taken to
read the Az/El positions and the limited accuracy of those
numbers in the header.

The sense of the clock error is such that it can be added
to the incorrect times in order to recover the actual time:

  Correct_time = Time_in_header + clock_error

=head1 AUTHOR

Tim Jenness E<lt>t.jenness@jach.hawaii.eduE<gt>

Copyright (C) UK Particle Physics and
Astronomy Research Council. All Rights Reserved.

=cut

