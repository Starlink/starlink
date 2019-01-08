#!/usr/bin/perl -w
#
# !!begin
# !!title  Header fixing script
# !!author T.R.Marsh
# !!created  14 January  2001
# !!revised  08 December 2005
# !!root   fixhead
# !!index  fixhead.pl
# !!descr  Perl script for fixing headers into standardised form for pamela and molly
# !!head1 Header fixing perl script
#
# !!emph{fixhead} gathers headers items into a standardised format and
# is required before running pamela and for final input into molly.
# You should use it before running the automatic reduce script. It is
# an attempt to place all the header dependent stuff into a single location.
# The automated reduce script for example will look for the object position,
# time and slit angle in order to decide whether to associate an arc with
# an object. It will look for these in the .more.pamela structure created
# by this script. Items not found are not created.
#
# !!head2 Invocation
# fixhead.pl night format file1 file2 @list1 @list2
#
# !!head2 Arguments
#
# !!table
# !!arg{ night}{  is an integer saying number of night of run.}
# !!arg{ format}{ is an integer to say which format the data has
# (see below for supported formats).}
# !!arg{file1, file2}{ are individual files.}
# !!arg{list1, list2}{ are ascii lists of files (flagged by @).}
# !!table
#
# !!head2 Suppoerted Formats
#
# Here are the supported formats. Try to choose the one nearest in
# time to your data on the same instrument.
#
# !!table
# !!trow { 1)}{ July 1998 WHT, normal and LSD}
# !!trow { 2)}{ December 1998 AAT}
# !!trow { 3)}{ January 2000 WHT}
# !!trow { 4)}{ March 2001 INT}
# !!trow { 5)}{ VLT UVES pipeline.}
# !!trow { 6)}{ March 2002 SAAO}
# !!trow { 7)}{ INT April 2003, 2009}
# !!trow { 8)}{ VLT FORS1 PMOS circular.}
# !!trow { 9)}{ VLT FORS2.}
# !!trow {10)}{ Magellan LDSS3.}
# !!trow {11)}{ Magellan IMACS.}
# !!trow {12)}{ SAAO Oct 2008}
# !!table
#
# OK, so you have tried all available formats but none of them work:
# you need to add your own. You will need to know perl to do so, and
# in particular be happy with regular expressions.
#
# To add a new format, look below for every place where the variable
# !!emph{$format} occurs. You need to add a line describing the format and
# change the range check on $format. Then you may need to change the
# way the headers are listed. To do this you need to play with 'hdstrace'
# to see how you can list your headers in such a way as to make it
# easy to grab the necessary items (see below for what items are required).
#
# An example is:
#
# hdstrace $file.more.fits nlines=all eachline
#
# which lists all the fits headers line by line.
#
# Next you need to add a series of statements to get hold of the
# particular header items from the list produced by the header listing.
# This is best done using Perl regular expression matches. The main thing
# to watch for is that the matches are unique. e.g. there may be an item
# called RA and one called CAT-RA and if you match to RA you could get
# either depending which comes second.
#
# Places to edit are delimited by START EDITING! and END EDITING!
#
# !!head2 Files produced
#
# The script will generate files called 'zzz_fixhead' and 'zzz_fixhead.log'
#
# !!end

use strict;

# Directories where various commands required are to be found
# Check existence of commands

# Top level of STARLINK

my $STAR     = $ENV{'STARLINK_DIR'};

# Check for existence of commands that will be used

-e "$STAR/bin/figaro/creobj" or die "Failed to find $STAR/bin/figaro/creobj.\n";
-e "$STAR/bin/figaro/setobj" or die "Failed to find $STAR/bin/figaro/setobj.\n";
-e "$STAR/bin/hdstrace"      or die "Failed to find $STAR/bin/hdstrace.\n";

# Names of files that will be created by running this routine.

my $script = 'zzz_fixhead';
my $log    = 'zzz_fixhead.log';

my $night = shift or die "No night number supplied.\n";
($night > 0) or die "Night number must be > 0\n";

my $format = shift;
($format >= 1 && $format <= 13)  or die
    "No format number supplied.\n"
    . "Formats available are:\n\n"
    . " 1) WHT July 1998, normal and LSD\n"
    . " 2) AAT December 1998\n"
    . " 3) WHT January 2000/01\n"
    . " 4) INT March 2001\n"
    . " 5) VLT UVES pipeline data\n"
    . " 6) SAAO March 2002\n"
    . " 7) INT April 2003, 2009\n"
    . " 8) VLT FORS1 PMOS circular, Oct 2003\n"
    . " 9) VLT FORS2, Mar 2004\n"
    . "10) Magellan LDSS3, April 2007 (JKT)\n"
    . "11) Magellan IMACS, September 2007 (JKT)\n"
    . "12) SAAO 72inch Oct 2008\n"
    . "13) WHT August 2018\n";

# Interpret file name arguments

my $nfiles = 0;
my $arg;
my @files;
while($arg = shift){
    if($arg =~ /^@(.*)/){
        my $list = $1;
        open(LIST, $list) or die "Can't open $list\n";
        while(<LIST>){
            chop($files[$nfiles++] = $_);
        }
        close(LIST);
    }else{
        $files[$nfiles++] = $arg;
    }
}

$nfiles or die "No file names loaded.\n";

if($nfiles == 1){
    print "\n$nfiles file name loaded.\n\n";
}else{
    print "\n$nfiles file names loaded.\n\n";
}

# Strip off trailing .sdf, check that files exist,
# are not directories, and are readable

my $file;
foreach $file (@files){
    $file =~ s/.sdf$//;
    -e "${file}.sdf" or die "${file}.sdf does not exist!\n";
    -f "${file}.sdf" or die "${file}.sdf is not a plain file!\n";
    -r "${file}.sdf" or die "${file}.sfd is not readable!\n";
    -w "${file}.sdf" or die "${file}.sdf is not writeable!\n";
}

my @mdays = (31,28,31,30,31,30,31,31,30,31,30,31);

# Generate a script to list header info. This part would have
# to be altered if the header was not in .MORE.FITS. By printing out
# the whole of the FITS header, the routine should be less sensitive
# to the exact posistion of items, but still may need varying according
# to the data format.

print "Listing headers to $log  ...\n\n";

open(SCRIPT,">$script") or die "Failed to open $script\n";
foreach $file (@files){

# START EDITING!
#
# All the formats supported so far are derived from FITS so have a
# .MORE.FITS structure with all the headers. Thus in all probability
# you just need to add another entry into the 'if' statement.
    if($format == 1 || $format == 2 || $format == 3 || $format == 4 ||
       $format == 5 || $format == 6 || $format == 7 || $format == 8 ||
       $format == 9 || $format == 10 || $format == 11 || $format == 12 ||
       $format == 13 ){
        print SCRIPT "echo File name = $file\n";
        print SCRIPT "$STAR/bin/hdstrace $file.more.fits nlines=all eachline\n";
    }else{
        die "Format number = $format has no header listing section defined!\n";
    }
# END EDITING!
}
close(SCRIPT);

# Run it

system("chmod +x $script; ./$script > $log");

# We need to obtain values for the following variables (all hashes
# keyed by file name)
#
# $object  = Object name. String of up to 32 characters.
# $date    = Date. String with format "dd/mm/yyyy"
# $night   = Night number. Integer, set above as second input.
# $numrun  = Run number.
# $time    = Exposure time (seconds)
# $utc     = Coordinated UT at mid-exposure, decimal hours.
# $ra      = right ascension in hours
# $dec     = declination in degrees
# $equinox = equinox in years.
# $slitpa  = angle of the slit on the sky
#
# and for format 8 (spectropol) the waveplate and wollaston prism
# angles are needed too.
#
# $jd, the JD at mid-exposure can substitute for $date and
# $utc since 'hfix' in molly can work with either.
#
# Other optional extras which can be added are
#
# $detector   the detector, e.g. name of the CCD
# $instrument description of the instrument
# $source     the source which should be set to one of ARC, FLAT, DATA, BIAS, DARK, SKY
#
# First open the log file and extract info. This part may well
# need altering depending upon the items available. The technique
# here is to use perl regular expression matching with particular
# parts isolated in brackets and then stored as $1, $2 etc. One
# has to be a little careful to get unique matches. A check is made
# for each item as we go.
#
# Note that the UT date can either be obtained by setting
# $date to a string of the form "21/03/1998" and $utc to
# a decimal time in hours OR by setting $jd to the julian day
# number.
#

# format pattern for floats
my $float  = "([\\+-]?\\d+(?:\\.\\d*)?(?:[Ee][-+]\\d+)?)";  # Any float

print "Grabbing header items ...\n\n";

open(LOG, $log) || die "Can't open $log\n";
my (%object, %numrun, %ra, %dec, %equinox, %jd, %utc, %date, %time, %slitpa, %reta, %woll, %source, %obstype);
my (%detector, %instrument);
my ($sign);
while(<LOG>){

    if(/^File name = (\S*)/){

        $file  = $1;

    }elsif($format == 1){

# WHT July 1998

        if(/\'OBJECT *=\s*\'([^']*)\'/){
            $object{$file} = $1;
        }elsif(/\'RUN *= *(\d*) .*Run number/){
            $numrun{$file} = $1;
        }elsif(/\'CAT-RA *=.* (\d\d):(\d\d):(\d\d\.\d*)/){
            $ra{$file} = $1+$2/60.+$3/3600.;
        }elsif(/\'CAT-DEC *=.*(.)(\d\d):(\d\d):(\d\d\.\d*)/){
            $sign = $1;
            $dec{$file}  = $2+$3/60.+$4/3600.;
            if($sign eq "-"){
                $dec{$file} *= -1.;
            }elsif($sign eq " "){
                print STDERR "WARNING: no sign found on declination in file = $file; assuming it to be positive\n";
            }elsif($sign ne "+"){
                die "Failed to recognise declinations sign = $sign in file = $file\n";
            }
        }elsif(/\'CAT-EQUI *=.*(\d\d\d\d\.\d*)/){
            $equinox{$file} = $1;
        }elsif(/\'UTSTART *= .*(\d\d):(\d\d):(\d\d\S*)/ && !defined $utc{$file}){
            $utc{$file} = $1 + $2/60. + $3/3600.;
        }elsif(/\'DATE[-\_]OBS= .*(\d\d)\/(\d\d)\/(\d\d)[^\d]/){
            $date{$file} = "$1/$2/19$3";
        }elsif(/\'EXPOSED\s*=\s*([\.\d]*)/ && !defined $time{$file}){
            $time{$file} = $1;
            print "found time = [",$1,"]\n";
        }elsif(/\'ROTSKYPA\s*=\s*$float\s*\//){
            $slitpa{$file} = $1;
        }elsif(/\'DETECTOR\s*=\s*\'\s*([^\']*?)\s*\'/){
            $detector{$file} = $1;
        }elsif(/\'INSTRUME\s*=\s*\'\s*([^\']*?)\s*\'/){
            $instrument{$file} = $1;
        }elsif(/\'OBSTYPE\s*=\s*\'\s*([^\']*?)\s*\'/){
            $source{$file} = $1;
        }elsif(/\'SPECTIME *= .*(\d\d):(\d\d):(\d\d\.\d*)/){
            $utc{$file} = $1 + $2/60. + $3/3600.;
        }elsif(/\'SPECINTT\s*=\s*([\.\d]+)/){
            $time{$file} = $1;
        }

    }elsif($format == 2){

# AAT Dec 1998

        if(/\'OBJECT *= \'(.*)\' *\//){
            $object{$file} = $1;
        }elsif(/\'RUN *= *(\d*) .*Run number/){
            $numrun{$file} = $1;
        }elsif(/\'MEANRA *= *(\d*\.\d*)/){
            $ra{$file} = $1/15.;
        }elsif(/\'MEANDEC *= *([-+]?\d*\.\d*)/){
            $dec{$file}  = $1;
        }elsif(/\'EQUINOX *= *(\d\d\d\d)/){
            $equinox{$file} = $1;
        }elsif(/\'UTSTART *= .*(\d\d):(\d\d):(\d\d\.\d\d)/){
            $utc{$file} = $1 + $2/60. + $3/3600.;
        }elsif(/\'UTDATE *= *\'(\d\d\d\d):(\d\d):(\d\d)/){
            $date{$file} = "$3/$2/$1";
        }elsif(/\'EXPOSED *= *(\d*)/){
            $time{$file} = $1;
        }

    }elsif($format == 3){

# WHT Jan 2000 & 2001

        if(/\'OBJECT *= \'(.*)\' \//){
            $object{$file} = $1;
        }elsif(/\'RUN *= *(\d*) .*Run number/){
            $numrun{$file} = $1;
        }elsif(/\'CAT-RA *=.* ([ \d]\d):(\d\d):(\d\d\.\d*)/){
            $ra{$file} = $1+$2/60.+$3/3600.;
        }elsif(/\'CAT-DEC *=.*(.)(\d\d):(\d\d):(\d\d\.\d*)/){
            $sign = $1;
            $dec{$file}  = $2+$3/60.+$4/3600.;
            if($sign eq "-"){
                $dec{$file} *= -1.;
            }elsif($sign eq " "){
                print STDERR "WARNING: no sign found on declination in file = $file; assuming it to be positive\n";
            }elsif($sign ne "+"){
                die "Failed to recognise declinations sign = $sign in file = $file\n";
            }
        }elsif(/\'CAT-EQUI *=.*(\d\d\d\d\.\d*)/){
            $equinox{$file} = $1;
        }elsif(/\'UTSTART *= .*(\d\d):(\d\d):(\d\d\S*)/){
            $utc{$file} = $1 + $2/60. + $3/3600.;
        }elsif(/\'DATE[-\_]OBS= .*(\d\d\d\d)-(\d\d)-(\d\d)/){
            $date{$file} = "$3/$2/$1";
        }elsif(/\'EXPOSED\s*=\s*([\.\d]+)/){
            $time{$file} = $1;
        }elsif(/\'ROTSKYPA\s*=\s*$float\s*\//){
            $slitpa{$file} = $1;
        }elsif(/\'DETECTOR\s*=\s*\'\s*([^\']*?)\s*\'/){
            $detector{$file} = $1;
        }elsif(/\'OBSTYPE\s*=\s*\'\s*([^\']*?)\s*\'/){
            $source{$file} = $1;
        }elsif(/\'INSTRUME\s*=\s*\'\s*([^\']*?)\s*\'/){
            $instrument{$file} = $1;
        }

    }elsif($format == 4){

# INT March 2001

        if(/\'OBJECT *= \'(.*)\' \//){
            $object{$file} = $1;
        }elsif(/\'RUN *= *(\d*) .*Run number/){
            $numrun{$file} = $1;
        }elsif(/\'CAT-RA *=.* (\d\d):(\d\d):(\d\d\.\d*)/){
            $ra{$file} = $1+$2/60.+$3/3600.;
        }elsif(/\'CAT-DEC *=.*(.)(\d\d):(\d\d):(\d\d\.\d*)/){
            $sign = $1;
            $dec{$file}  = $2+$3/60.+$4/3600.;
            if($sign eq "-"){
                $dec{$file} *= -1.;
            }elsif($sign eq " "){
                print STDERR "WARNING: no sign found on declination in file = $file; assuming it to be positive\n";
            }elsif($sign ne "+"){
                die "Failed to recognise declinations sign = $sign in file = $file\n";
            }
        }elsif(/\'CAT-EQUI *=.*(\d\d\d\d\.\d*)/){
            $equinox{$file} = $1;
        }elsif(/\'EXPOSED *= *(\d*)/){
            $time{$file} = $1;
        }elsif(/\'JD *= *(24\d\d\d\d\d\.\d\d\d\d\d\d)/){
            $jd{$file} = $1;
        }elsif(/\'ROTSKYPA\s*=\s*$float\s*\//){
            $slitpa{$file} = $1;
        }

    }elsif($format == 5){

# VLT UVES pipeline data

        if(/\'OBJECT\s*=\s*\'(.*)\'\s/){
            $object{$file} = $1;
        }elsif(/\'OS-EXPOI\s*=\s*(\d*)\s/){
            $numrun{$file} = $1;
        }elsif(/\'RA\s*=\s*(\S*)\s/){
            $ra{$file} = $1/15.;
        }elsif(/\'DEC\s*=\s*(\S*)\s/){
            $dec{$file} = $1;
        }elsif(/\'EQUINOX\s*=\s*(\S*)\s/){
            $equinox{$file} = $1;
        }elsif(/\'EXPTIME\s*=\s*(\S*)\s/){
            $time{$file} = $1;
        }elsif(/\'MJD-OBS\s*=\s*(\S*)\s/){
            $jd{$file} = $1+2400000.5;
        }

    }elsif($format == 6){

# SAAO

        if(/\'OBJECT *= \'(.*)\' *\//){
            $object{$file} = $1;
        }elsif(/\'RA\s*=\s'(\d\d):(\d\d):(\d\d)'/){
            $ra{$file} = $1/15.;
        }elsif(/\'DEC\s*=\s*'(.)(\d\d):(\d\d):(\d\d)'/){
            $sign = $1;
            $dec{$file}  = $2+$3/60.+$4/3600.;
            if($sign eq "-"){
                $dec{$file} *= -1.;
            }elsif($sign eq " "){
                print STDERR "WARNING: no sign found on declination in file = $file; assuming it to be positive\n";
            }elsif($sign ne "+"){
                die "Failed to recognise declinations sign = $sign in file = $file\n";
            }
        }elsif(/\'EQUINOX\s*=\s*(\d\d\d\d)/){
            $equinox{$file} = $1;
        }elsif(/\'TIME-OBS\s*=\s'(\d\d):(\d\d):(\d\d)'/){
            $utc{$file} = $1 + $2/60. + $3/3600.;
        }elsif(/\'DATE-OBS\s*=\s\'(\d\d\d\d)-(\d\d)-(\d\d)/){
            $date{$file} = "$3/$2/$1";
        }elsif(/\'EXPTIME\s*=\s*(\S*)/){
            $time{$file} = $1;
        }

    }elsif($format == 7){

# INT April 2003

        if(/\'OBJECT *= \'(.*)\' \//){
            $object{$file} = $1;
        }elsif(/\'RUN *= *(\d*) .*Run number/){
            $numrun{$file} = $1;
        }elsif(/\'CAT-RA *=.* (\d+):(\d\d):(\d\d\.\d*)/){
            $ra{$file} = $1+$2/60.+$3/3600.;
        }elsif(/\'CAT-DEC *=.*(.)(\d\d):(\d\d):(\d\d\.\d*)/){
            $sign = $1;
            $dec{$file}  = $2+$3/60.+$4/3600.;
            if($sign eq "-"){
                $dec{$file} *= -1.;
            }elsif($sign eq " "){
                print STDERR "WARNING: no sign found on declination in file = $file; assuming it to be positive\n";
            }elsif($sign ne "+"){
                die "Failed to recognise declinations sign = $sign in file = $file\n";
            }
        }elsif(/\'CAT-EQUI *=.*(\d\d\d\d\.\d*)/){
            $equinox{$file} = $1;
        }elsif(/\'EXPOSED *= *(\d*)/){
            $time{$file} = $1;
        }elsif(/\'UTSTART *= .*(\d\d):(\d\d):(\d\d\S*)/){
            $utc{$file} = $1 + $2/60. + $3/3600.;
        }elsif(/\'DATE-OBS\s*=\s\'(\d\d\d\d)-(\d\d)-(\d\d)/){
            $date{$file} = "$3/$2/$1";
        }elsif(/\'ROTSKYPA\s*=\s*$float\s*\//){
            $slitpa{$file} = $1;
        }

    }elsif($format == 8){

# VLT FORS1 PMOS

        if(/\'HIERARCH ESO OBS TARG NAME *= \'(.*)\' *\//){
            $object{$file} = $1;
        }elsif(/\'HIERARCH ESO DET EXP NO *= *(\d*)/){
            $numrun{$file} = $1;
        }elsif(/\'RA *=.* (\d*\.\d*)/){
            $ra{$file} = $1/15.;
        }elsif(/\'DEC *=  *(.\d*\.\d*)/){
            $dec{$file}  = $1;
        }elsif(/\'EQUINOX *=.*(\d\d\d\d\.\d*)/){
            $equinox{$file} = $1;
        }elsif(/\'EXPTIME *= *(\d*\.\d*)/){
            $time{$file} = $1;
        }elsif(/\'MJD-OBS *= *(\d\d\d\d\d\.\d\d*)/){
            $jd{$file} = 2400000.5+$1;
        }elsif(/\'HIERARCH ESO INS RETA4 POSANG *= *(-?\d*\.\d*)/){
            $reta{$file} = $1;
        }elsif(/\'HIERARCH ESO INS WOLL POSANG *= *(-?\d*\.\d*)/){
            $woll{$file} = $1;
        }elsif(/\'HIERARCH ESO ADA POSANG\s*=\s*(-?\d*\.\d*)/){
            $slitpa{$file} = $1;
        }

    }elsif($format == 9){

# VLT FORS2, Mar 2004

        if(/\'HIERARCH ESO OBS TARG NAME *= \'(.*)\' *\//){
            $object{$file} = $1;
        }elsif(/\'HIERARCH ESO DPR TYPE *= \'(.*)\' *\//){
            $obstype{$file} = $1;
            my $source = $1;
            if($source =~ /sky/i){
                $source{$file} = "data";
            }elsif($source =~ /flat/i){
                $source{$file} = "flat";
            }elsif($source =~ /wave/i){
                $source{$file} = "arc";
            }elsif($source =~ /bias/i){
                $source{$file} = "bias";
            }
        }elsif(/\'HIERARCH ESO DET EXP NO *= *(\d*)/){
            $numrun{$file} = $1;
        }elsif(/\'RA *=.* (\d*\.\d*)/){
            $ra{$file} = $1/15.;
        }elsif(/\'DEC *=  *(.\d*\.\d*)/){
            $dec{$file}  = $1;
        }elsif(/\'EQUINOX *=.*(\d\d\d\d\.\d*)/){
            $equinox{$file} = $1;
        }elsif(/\'EXPTIME *= *(\d*\.\d*)/){
            $time{$file} = $1;
        }elsif(/\'MJD-OBS *= *(\d\d\d\d\d\.\d\d*)/){
            $jd{$file} = 2400000.5+$1;
        }elsif(/\'HIERARCH ESO ADA POSANG\s*=\s*(-?\d*\.\d*)/){
            $slitpa{$file} = $1;
        }

# Magellan LDSS3, April 2007 (JKT)

    }elsif($format == 10){

        if(/\'OBJECT *= \'(.*)\' *\//){
            $object{$file} = $1;
        }elsif(/\'EXPTYPE *= \'(.*)\' *\//){
            $obstype{$file} = $1;
            my $source = $1;
            if($source =~ /sky/i){
                $source{$file} = "data";
            }elsif($source =~ /flat/i){
                $source{$file} = "flat";
            }elsif($source =~ /wave/i){
                $source{$file} = "arc";
            }elsif($source =~ /bias/i){
                $source{$file} = "bias";
            }
        }elsif(/\'FILENAME= *\'ccd(\d*)c\d/){
            $numrun{$file} = $1;
        }elsif(/\'RA-D *=.* (\d*\.\d*)/){
            $ra{$file} = $1/15.;
        }elsif(/\'DEC-D *=  *(.\d*\.\d*)/){
            $dec{$file}  = $1;
        }elsif(/\'EQUINOX *=.*(\d\d\d\d\.\d*)/){
            $equinox{$file} = $1;
        }elsif(/\'EXPTIME *= *(\d*\.\d*)/){
            $time{$file} = $1;
        }elsif(/\'DATE-OBS\s*=\s\'(\d\d\d\d)-(\d\d)-(\d\d)/){
            $date{$file} = "$3/$2/$1";
        }elsif(/\'UT-TIME\s*=\s'(\d\d):(\d\d):(\d\d)'/){
            $utc{$file} = $1 + $2/60. + $3/3600.;
        }elsif(/\'ROTANGLE\s*=\s*(-?\d*\.\d*)/){
            $slitpa{$file} = $1;
#         }elsif(/\'DETECTOR\s*=\s*\'\s*([^\']*?)\s*\'/){
#              $detector{$file} = $1;
        }elsif(/\'INSTRUME\s*=\s*\'\s*([^\']*?)\s*\'/){
            $instrument{$file} = $1;
        }

# Magellan IMACS, September 2007 (JKT)

    }elsif($format == 11){

        if(/\'OBJECT *= \'(.*)\' *\//){
            $object{$file} = $1;
        }elsif(/\'EXPTYPE *= \'(.*)\' *\//){
            $obstype{$file} = $1;
            my $source = $1;
            if($source =~ /sky/i){
                $source{$file} = "data";
            }elsif($source =~ /flat/i){
                $source{$file} = "flat";
            }elsif($source =~ /wave/i){
                $source{$file} = "arc";
            }elsif($source =~ /bias/i){
                $source{$file} = "bias";
            }
        }elsif(/\'FILENAME= *\'ccd(\d*)c\d/){
            $numrun{$file} = $1;
        }elsif(/\'RA-D *=.* (\d*\.\d*)/){
            $ra{$file} = $1/15.;
        }elsif(/\'DEC-D *=  *(.\d*\.\d*)/){
            $dec{$file}  = $1;
        }elsif(/\'EQUINOX *=.*(\d\d\d\d\.\d*)/){
            $equinox{$file} = $1;
        }elsif(/\'EXPTIME *= *(\d*\.\d*)/){
            $time{$file} = $1;
        }elsif(/\'DATE-OBS\s*=\s\'(\d\d\d\d)-(\d\d)-(\d\d)/){
            $date{$file} = "$3/$2/$1";
        }elsif(/\'UT-TIME\s*=\s'(\d\d):(\d\d):(\d\d)'/){
            $utc{$file} = $1 + $2/60. + $3/3600.;
        }elsif(/\'ROTANGLE\s*=\s*(-?\d*\.\d*)/){
            $slitpa{$file} = $1;
        }

    }elsif($format == 12){

# SAAO 2008

        $slitpa{$file} = 0.0 ;
        $numrun{$file} = int(substr($file,-1,4));
        if(/\'OBJECT *= \'(.*)\' *\//){
            $object{$file} = $1;
        }elsif(/\'RA\s*=\s'\s*(\d\d):(\d\d):(\d\d)'/){
            $ra{$file} = $1+$2/60.+$3/3600.;
        }elsif(/\'DEC\s*=\s*'(.)(\d\d):(\d\d):(\d\d)'/){
            $sign = $1;
            $dec{$file}  = $2+$3/60.+$4/3600.;
            if($sign eq "-"){
                $dec{$file} *= -1.;
            }elsif($sign eq " "){
                print STDERR "WARNING: no sign found on declination in file = $file; assuming it to be positive\n";
            }elsif($sign ne "+"){
                die "Failed to recognise declinations sign = $sign in file = $file\n";
            }
        }elsif(/\'EPOCH\s*=\s*(\d\d\d\d)/){
            $equinox{$file} = $1;
        }elsif(/\'UT\s*=\s'(\d\d):(\d\d):(\d\d)'/){
            $utc{$file} = $1 + $2/60. + $3/3600.;
        }elsif(/\'DATE-OBS\s*=\s\'(\d\d\d\d)-(\d\d)-(\d\d)/){
            $date{$file} = "$3/$2/$1";
        }elsif(/\'EXPTIME\s*=\s*(\S*)/){
            $time{$file} = $1;
        }

    }elsif($format == 13){

# WHT Aug 2018

        if(/\'OBJECT *= \'(.*)\' \//){
            $object{$file} = $1;
        }elsif(/\'RUN *= *(\d*) .*Run number/){
            $numrun{$file} = $1;
        }elsif(/\'CAT-RA *= *'([ \d]\d):(\d\d):(\d\d\.\d*)/){
            $ra{$file} = $1+$2/60.+$3/3600.;
        }elsif(/\'CAT-DEC *= *'(.)(\d\d):(\d\d):(\d\d\.\d*)/){
            $sign = $1;
            $dec{$file}  = $2+$3/60.+$4/3600.;
            if($sign eq "-"){
                $dec{$file} *= -1.;
            }elsif($sign eq " "){
                print STDERR "WARNING: no sign found on declination in file = $file; assuming it to be positive\n";
            }elsif($sign ne "+"){
                die "Failed to recognise declinations sign = $sign in file = $file\n";
            }
        }elsif(/\'CAT-EQUI *=.*(\d\d\d\d\.\d*)/){
            $equinox{$file} = $1;
        }elsif(/\'UTSTART *= .*(\d\d):(\d\d):(\d\d\S*)/){
            $utc{$file} = $1 + $2/60. + $3/3600.;
        }elsif(/\'DATE[-\_]OBS= .*(\d\d\d\d)-(\d\d)-(\d\d)/){
            $date{$file} = "$3/$2/$1";
        }elsif(/\'EXPOSED\s*=\s*([\.\d]+)/){
            $time{$file} = $1;
        }elsif(/\'ROTSKYPA\s*=\s*$float\s*\//){
            $slitpa{$file} = $1;
        }elsif(/\'DETECTOR\s*=\s*\'\s*([^\']*?)\s*\'/){
            $detector{$file} = $1;
        }elsif(/\'OBSTYPE\s*=\s*\'\s*([^\']*?)\s*\'/){
            $source{$file} = $1;
        }elsif(/\'INSTRUME\s*=\s*\'\s*([^\']*?)\s*\'/){
            $instrument{$file} = $1;
        }

# START EDITING!
# insert new formats here
# END EDITING!

    }else{
        die "Regular expressions for parameter extraction not defined for format = $format!\n";
    }
}
close(LOG);

# Generate a script to create and set header info.

print "Fixing up headers ...\n\n";

open(SCRIPT,">$script") || die "Failed to open $script\n";

print SCRIPT "#!/bin/csh -f\n\n";

# Create items (if they are already present then this generates
# non-fatal errors when run).

foreach $file (@files){

# Create basic .more.pamela structure


    print SCRIPT "$STAR/bin/figaro/creobj Ext 0 $file.more\n";
    print SCRIPT "$STAR/bin/figaro/creobj Struct 0 $file.more.pamela\n";

# object name

    if(defined $object{$file}){
        print SCRIPT "$STAR/bin/figaro/creobj '_CHAR*32' 0 $file.more.pamela.object\n";
        $object{$file} =~ s/^\s*//;
        $object{$file} =~ s/\s*$//;
        print SCRIPT "$STAR/bin/figaro/setobj \\'\"$object{$file}\"\\' $file.more.pamela.object\n";
    }elsif(defined $obstype{$file}){
        print SCRIPT "$STAR/bin/figaro/creobj '_CHAR*32' 0 $file.more.pamela.object\n";
        ($object{$file} = $obstype{$file}) =~ s/^\s*//;
        $object{$file} =~ s/\s*$//;
        print SCRIPT "$STAR/bin/figaro/setobj \\'$object{$file}\\' $file.more.pamela.object\n";
    }else{
        print "'object' undefined for $file\n";
    }

# Now the time. For each format we need to know exactly what the time represents
# start of exposure, middle of exposure or something else altogether

# START EDITING!
    if($format == 1  || $format == 2  || $format == 3 || $format == 5 ||
       $format == 6  || $format == 7  || $format == 8 || $format == 9 ||
       $format == 10 || $format == 11 || $format == 12 || $format == 13) {
        if(defined $jd{$file}){
            if(defined $time{$file}) {
                $jd{$file} += ($time{$file} / 7200. / 24.);
                print "Assuming that JD grabbed represents the START of exposure of file = $file\n";
            }else{
                print "JD found in $file, but not exposure time, so JD cannot be corrected\n";
                print "and will therefore be UNDEFINED for safety\n";
                undef $jd{$file};
            }
        }elsif(defined $utc{$file}){
            if(defined $time{$file}){
                $utc{$file} += $time{$file}/7200.;
                if($utc{$file} >= 24.0){
                    if($date{$file} =~ /(\d\d)\/(\d\d)\/(\d\d\d\d)/){
                        $utc{$file} -= 24.;
                        my ($day, $month, $year, $nday);
                        $day   = $1;
                        $month = $2;
                        $year  = $3;
                        $day++;
                        $nday  = $mdays[$month-1];
                        if($year % 4 == 0 && $month == 2){
                            $nday++;
                        }
                        if($day > $nday){
                            $month++;
                            $day = 1;
                        }
                        if($month > 12){
                            $month = 1;
                            $year++;
                        }
                        $date{$file} = sprintf("%02d/%02d/%04d", $day, $month, $year);
                    }else{
                        die "UTC > 24 but date cannot be corrected.\n";
                    }
                }

                print "Assuming that UT grabbed represents the START of exposure of file = $file\n";
            }else{
                print "UTC found in $file, but not exposure time, so UTC cannot be corrected\n";
                print "and will therefore be UNDEFINED for safety\n";
                undef $utc{$file};
            }
        }
    }elsif($format == 4){
        if(defined $jd{$file}){
            print "Assuming that the JD grabbed represents the MIDDLE of exposure...\n";
        }elsif(defined $utc{$file}){
            print "Assuming that UT grabbed represents the MIDDLE of exposure...\n";
        }
    }else{
        die "No correction to the centre of the exposure specified for format = $format!\n";
    }
# END EDITING!

    if(defined $jd{$file}){
        print SCRIPT "$STAR/bin/figaro/creobj '_DOUBLE' 0 $file.more.pamela.jd\n";
        print SCRIPT "$STAR/bin/figaro/setobj $jd{$file} $file.more.pamela.jd\n";
    }elsif(defined $utc{$file} && defined $date{$file}){
        print SCRIPT "$STAR/bin/figaro/creobj '_CHAR*10' 0 $file.more.pamela.date\n";
        print SCRIPT "$STAR/bin/figaro/creobj '_CHAR*12' 0 $file.more.pamela.utc\n";
        print SCRIPT "$STAR/bin/figaro/setobj \\'$date{$file}\\' $file.more.pamela.date\n";
        my $uth = int($utc{$file});
        my $utm = int(60.*($utc{$file}-$uth));
        my $uts = 3600.*($utc{$file}-$uth)-60.*$utm;
        printf SCRIPT "$STAR/bin/figaro/setobj %2.2d:%2.2d:%06.3f $file.more.pamela.utc\n",$uth,$utm,$uts;
    }else{
        print "ERROR: no time found in file = $file\n";
    }

# Night number must be defined

    print SCRIPT "$STAR/bin/figaro/creobj '_INTEGER' 0 $file.more.pamela.night\n";
    print SCRIPT "$STAR/bin/figaro/setobj $night   $file.more.pamela.night\n";

    if(defined $numrun{$file}){
        print SCRIPT "$STAR/bin/figaro/creobj '_INTEGER' 0 $file.more.pamela.numrun\n";
        print SCRIPT "$STAR/bin/figaro/setobj $numrun{$file}  $file.more.pamela.numrun\n";
    }else{
        print "Run number not defined for file = $file\n";
    }

    if(defined $time{$file}){
        print SCRIPT "$STAR/bin/figaro/creobj '_REAL' 0 $file.more.pamela.time\n";
        print SCRIPT "$STAR/bin/figaro/setobj $time{$file}    $file.more.pamela.time\n";
    }else{
        print "Exposure time not defined for file = $file\n";
    }

    if(defined $ra{$file} && defined $dec{$file} && defined $equinox{$file}){
        print SCRIPT "$STAR/bin/figaro/creobj '_DOUBLE' 0 $file.more.pamela.ra\n";
        print SCRIPT "$STAR/bin/figaro/setobj $ra{$file}      $file.more.pamela.ra\n";
        print SCRIPT "$STAR/bin/figaro/creobj '_DOUBLE' 0 $file.more.pamela.dec\n";
        print SCRIPT "$STAR/bin/figaro/setobj $dec{$file}     $file.more.pamela.dec\n";
        print SCRIPT "$STAR/bin/figaro/creobj '_DOUBLE' 0 $file.more.pamela.equinox\n";
        print SCRIPT "$STAR/bin/figaro/setobj $equinox{$file} $file.more.pamela.equinox\n";
    }else{
        print "One or more of ra, dec and equinox undefined for file = $file, so none of them saved\n";
    }

    if(defined $slitpa{$file}){
        print SCRIPT "$STAR/bin/figaro/creobj '_REAL' 0 $file.more.pamela.slitpa\n";
        print SCRIPT "$STAR/bin/figaro/setobj $slitpa{$file}    $file.more.pamela.slitpa\n";
    }else{
        print "Slit PA time not defined for file = $file\n";
    }


    if(defined $source{$file}){
        if($source{$file} =~ /flat/i){
            print SCRIPT "$STAR/bin/figaro/creobj '_CHAR*4' 0 $file.more.pamela.source\n";
            print SCRIPT "$STAR/bin/figaro/setobj FLAT        $file.more.pamela.source\n";
        }elsif($source{$file} =~ /dark/i){
            print SCRIPT "$STAR/bin/figaro/creobj '_CHAR*4' 0 $file.more.pamela.source\n";
            print SCRIPT "$STAR/bin/figaro/setobj DARK        $file.more.pamela.source\n";
        }elsif($source{$file} =~ /bias/i){
            print SCRIPT "$STAR/bin/figaro/creobj '_CHAR*4' 0 $file.more.pamela.source\n";
            print SCRIPT "$STAR/bin/figaro/setobj BIAS        $file.more.pamela.source\n";
        }elsif($source{$file} =~ /arc/i){
            print SCRIPT "$STAR/bin/figaro/creobj '_CHAR*4' 0 $file.more.pamela.source\n";
            print SCRIPT "$STAR/bin/figaro/setobj ARC         $file.more.pamela.source\n";
        }elsif($source{$file} =~ /data/i || $source{$file} =~ /target/i){
            print SCRIPT "$STAR/bin/figaro/creobj '_CHAR*4' 0 $file.more.pamela.source\n";
            print SCRIPT "$STAR/bin/figaro/setobj DATA        $file.more.pamela.source\n";
        }elsif($source{$file} =~ /sky/i){
            print SCRIPT "$STAR/bin/figaro/creobj '_CHAR*4' 0 $file.more.pamela.source\n";
            print SCRIPT "$STAR/bin/figaro/setobj SKY         $file.more.pamela.source\n";
        }
    }

    if(defined $detector{$file}){
        print SCRIPT "$STAR/bin/figaro/creobj '_CHAR*20' 0 $file.more.pamela.detector\n";
        print SCRIPT "$STAR/bin/figaro/setobj \\'$detector{$file}\\' $file.more.pamela.detector\n";
    }

    if(defined $instrument{$file}){
        print SCRIPT "$STAR/bin/figaro/creobj '_CHAR*20' 0 $file.more.pamela.instrument\n";
        print SCRIPT "$STAR/bin/figaro/setobj \\'$instrument{$file}\\' $file.more.pamela.instrument\n";
    }


# Special section for the VLT spectropolarimetry

    if($format == 8){
        if(defined $reta{$file} && $woll{$file}){
            my $angle = $woll{$file} - $reta{$file};
            print SCRIPT "$STAR/bin/figaro/creobj Struct 0 $file.more.pamela.vlt_fors1\n";
            print SCRIPT "$STAR/bin/figaro/creobj '_DOUBLE' 0 $file.more.pamela.vlt_fors1.angle\n";
            print SCRIPT "$STAR/bin/figaro/setobj $angle $file.more.pamela.vlt_fors1.angle\n";
        }else{
            print "Either or both of reta and woll not defined for file = $file\n";
        }
    }

# format code stored in case of problems
    print SCRIPT "$STAR/bin/figaro/creobj '_INTEGER' 0 $file.more.pamela.format\n";
    print SCRIPT "$STAR/bin/figaro/setobj $format   $file.more.pamela.format\n";

}
close(SCRIPT);

# Run it

system("chmod +x $script; ./$script > $log");

print "Finished!!\n\n";

exit;




