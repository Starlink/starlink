#!/usr/bin/perl -w
#
# !!begin
# !!title    Automatic reduction script
# !!author   T.R. Marsh
# !!created  14 January 2001
# !!revised  13 August 2009
# !!root     reduce
# !!index    reduce.pl
# !!descr    Perl script for automated reduction
# !!head1    Automated reduction perl script
#
# This script carries out complete reduction of a series of
# files. It handles identification of which are arcs and which
# are objects, and extracts normally and optimally and also
# extracts sky spectra at the position of the object. The user must
# have set up flat fields, a file defining the sky regions and various other parameters.
#
# The arcs (and optionally flats) to be associated with a given object are determined
# from the position, time and slit angle. If these are not found, the script will
# issue a warning and take an inclusive approach. i.e. It will assume that
# an arc is associated unless it can prove that it is not.
#
# NB the script !!ref{fixhead.html}{fixhead} must have been run on all the files
# before using reduce. This creates a standardized header structure.
#
# !!head2 Invocation
#
# reduce.pl datafile (switches) file1 file2 @list1 @list2
#
# where file1, file2 are individual files, while list1 and list2 are
# lists of file names
#
# !!head2 Switches
#
# There are two switches to modify the action of !!emph{reduce}:
#
# !!table
# !!arg{-t}{If specified the list of data types will be wriiten but no reduction will be carried out.}
# !!arg{-s}{If specified, the determination of data types will be skipped. The combination of -t and -s allows you
# to have one run to determine types, which you can edit, and then run with -s to ensure that it
# does not re-determine the data types. Note that you cannot specify both options simultaneously.}
# !!table
#
# !!head2 Action
#
# Given a list of files, !!emph{reduce} first generates a script to
# extract positional and timing data and also to analyse the frames
# to identify data type using idtype or optionally the headers for
# format 5 at least. The frames are classified as data, arc, flat, or
# junk and the latter are ignored (and warnings are issued).
#
# It then generates and runs the pamela reduction commands.
#
# !!head2 What reduce does NOT do
#
# 'reduce' does NOT debias the data. You must debias the data before
# running reduce.
#
# !!head2 Setting up for using !!emph{reduce}
#
# A fair bit of setting up is needed to use reduce effectively. You are going to have
# to decide on the parameters you want to use for reduction. These need to
# be edited into a separate file -- see !!ref{datafile}{datafile} for a
# template. The best way to set the parameters is to go through some
# reduction by hand. This will allow you to decide such parameters as
# the number of coefficients for fitting the sky background etc. You will
# also need to set up various files such as a unit and zeroframe, a master
# sky frame and work out a reference position for skymov.
#
# If you are running on a new set of data you may need to add some code
# to both this script and the header fixing script.
#
# Once you have done this and set up the data file to your satisfaction
# then you can try running this script.
#
# !!head2 Files produced by !!emph{reduce}
#
# !!emph{reduce} produces many temporary files. These have different name, but all start
# with !!emph{zzz}, so just delete zzz* to get rid of them. In some cases they may be of
# use when problems occur which is why they are not deleted automatically.
#
# !!head2 Datafile format
#
# Even if you don't have to edit the script file, you will undoubtedly have
# to edit your own !!ref{datafile}{datafile}. So download it using shift-click
# and edit it. Mostly it consists of setting parameters used by the pamela routines.
# You may need to change the format number if you have edited the script for your format.
#
# !!end

use strict;
use warnings;

my $version = "12/12/2005";

# save command arguments:

my $sargs = join(' ',@ARGV);

# Process rest of arguments, reading lists, filenames and setting flags

my $types           = 0;
my $skip            = 0;

my $narg = 0;
my ($arg, @files, $datafile);
while($arg = shift){
    if($arg eq '-t'){
	$types  = 1;
    }elsif($arg eq '-s'){
	$skip  = 1;
    }elsif($narg == 0){
	$datafile  = $arg;
	$narg++;
    }elsif($arg =~ /@(.*)/){
	my $list = $1;
	open(LIST, $list) or die "Can't open $list\n";
	while(<LIST>){
	    chop $_;
	    push @files, $_;
	}
	close(LIST);
    }elsif($arg !~ /^\s*$/){
	push @files, $arg;
    }
}

(!$types || !$skip) or die "You can't both make and not make the datatypes file. Please review the meaning of the command line options\n";

defined $datafile or die "No datafile was defined\n";

open(DATAFILE, $datafile) || die "Failed to open $datafile.\n";

# Items are recognised as contiguous strings starting after
# an = sign and ending with a space. They must not start with
# a # to prevent blank items being interpreted. Rudimentary checks
# on format are also applied. e.g. integers must only have numbers.
#
# Format patterns:

my $pint   = "(\\+?\\d+)";				    # Positive integers
my $nint   = "(-\\d+)";					    # Negative integers
my $int    = "([\\+-]?\\d+)";				    # Any integer
my $opint  = "(\\+?\\d*[13579])";			    # Positive odd integer
my $pfloat = "(\\+?\\d+(?:\\.\\d*)?(?:[Ee][-+]\\d+)?)";	    # Positive float
my $nfloat = "(-\\d+(?:\\.\\d*)?(?:[Ee][-+]\\d+)?)";	    # Negative float
my $float  = "([\\+-]?\\d+(?:\\.\\d*)?(?:[Ee][-+]\\d+)?)";  # Any float
my $string = "([^\\s\\#]\\S+)";				    # string

# the parameters to be read from the file are many ...
my ($dversion, $balance, $zero, $unit, $master_sky, $data_types, $reduce_script, $reduce_log, @extra_files);
my ($arc_pos_tol, $arc_pa_tol, $arc_time_tol, $xstart, $ystart, $xend, $yend, $readout, $bias_region, $photon);
my ($track_npoly, $track_nspline, $track_xstart, $track_xend, $track_template, $track_tweak, $track_offset, $track_fchange,
    $track_nobj, $track_iobj, $track_iblock, $track_ypos, $track_width, $track_esig, $track_fwhm, $track_clip,
    $track_nblock, $track_psig, $track_tcycle, $track_change);
my ($profit_npoly, $profit_sizex, $profit_thresh, $profit_nslow, $profit_nblock, $profit_nmed, $profit_badval);
my ($ident, $nearflat, $twilight, $maskfile, $flat_pos_tol, $flat_pa_tol, $flat_time_tol, $flat_npoly, $flat_thrlo,
    $flat_thrhi, $flat_ncycle, $dichroic_ylo, $dichroic_yhi, $dichroic_balance);
my ($skymov_slo, $skymov_shi, $skymov_fwhm, $skymov_xpos, $skymov_npoly, $skymov_tlo, $skymov_thi, $skymov_nmin,
    $skymov_nwidth, $skyfit_npoly, $skyfit_thresh, $sky_mask);
my ($extopt_npoly, $extopt_thresh, $extopt_nblock, $extopt_nmed, $extopt_ratlim, $extopt_eps);
my ($optext_ratlim, $optext_eps, $optext_iave);
my ($idtype_track, $idtype_nxwidth, $idtype_nywidth, $idtype_tobjm, $idtype_tobja, $idtype_tlinm, $idtype_tlina, $idtype_flim);

# ok now read
while(<DATAFILE>){
    if(/^\s*version\s*=\s*$string/){
	$dversion = $1;
    }elsif(/^\s*balance\s*=\s*$string/){
	$balance = $1;
    }elsif(/^\s*zero\s*=\s*$string/){
	$zero = $1;
    }elsif(/^\s*unit\s*=\s*$string/){
	$unit = $1;
    }elsif(/^\s*master\s*sky\s*=\s*$string/){
	$master_sky = $1;
    }elsif(/^\s*data\s*types\s*=\s*$string/){
	$data_types = $1;
    }elsif(/^\s*reduce\s*script\s*=\s*$string/){
	$reduce_script = $1;
    }elsif(/^\s*reduce\s*log\s*=\s*$string/){
	$reduce_log = $1;
    }elsif(/^\s*files\s*=\s*(.*)\s*\#/){
	@extra_files = split(' ', $1);
    }elsif(/^\s*arc_pos_tolerance\s*=\s*$pfloat/){
	$arc_pos_tol = $1;
    }elsif(/^\s*arc_pa_tolerance\s*=\s*$pfloat/){
	$arc_pa_tol = $1;
    }elsif(/^\s*arc_time_tolerance\s*=\s*$pfloat/){
	$arc_time_tol = $1;
    }elsif(/^\s*xstart\s*=\s*$pint/){
	$xstart = $1;
    }elsif(/^\s*xend\s*=\s*$pint/){
	$xend = $1;
    }elsif(/^\s*ystart\s*=\s*$pint/){
	$ystart = $1;
    }elsif(/^\s*yend\s*=\s*$pint/){
	$yend = $1;
    }elsif(/^\s*readout\s*=\s*$pfloat/){
	$readout = $1;
    }elsif(/^\s*bias_region\s*=\s*$string/){
	$bias_region = $1;
    }elsif(/^\s*photon\s*=\s*$pfloat/){
	$photon = $1;
    }elsif(/^\s*track_nspline\s*=\s*$pint/){
        $track_nspline = $1;
    }elsif(/^\s*track_npoly\s*=\s*$pint/){
        $track_npoly = $1;
    }elsif(/^\s*track_xstart\s*=\s*$pint/){
        $track_xstart = $1;
    }elsif(/^\s*track_xend\s*=\s*$pint/){
        $track_xend = $1;
    }elsif(/^\s*track_template\s*=\s*$string/){
        $track_template = $1;
    }elsif(/^\s*track_tweak\s*=\s*(true|false)/){
        $track_tweak = $1;
    }elsif(/^\s*track_offset\s*=\s*$float/){
        $track_offset = $1;
    }elsif(/^\s*track_fchange\s*=\s*$pfloat/){
        $track_fchange = $1;
    }elsif(/^\s*track_nobj\s*=\s*$pint/){
        $track_nobj = $1;
    }elsif(/^\s*track_iobj\s*=\s*$pint/){
        $track_iobj = $1;
    }elsif(/^\s*track_iblock\s*=\s*$pint/){
        $track_iblock = $1;
    }elsif(/^\s*track_ypos\s*=\s*$pfloat/){
        $track_ypos = $1;
    }elsif(/^\s*track_width\s*=\s*$pint/){
        $track_width = $1;
    }elsif(/^\s*track_esig\s*=\s*$pfloat/){
        $track_esig = $1;
    }elsif(/^\s*track_fwhm\s*=\s*$pfloat/){
        $track_fwhm = $1;
    }elsif(/^\s*track_clip\s*=\s*$pfloat/){
        $track_clip = $1;
    }elsif(/^\s*track_nblock\s*=\s*$pint/){
        $track_nblock = $1;
    }elsif(/^\s*track_psig\s*=\s*$pfloat/){
        $track_psig = $1;
    }elsif(/^\s*track_tcycle\s*=\s*$pint/){
        $track_tcycle = $1;
    }elsif(/^\s*track_change\s*=\s*$pfloat/){
        $track_change = $1;
    }elsif(/^\s*identifier\s*=/){
	if(/^\s*identifier\s*=\s*$string/){
	    $ident = $1;
	}else{
	    $ident = "";
	}
    }elsif(/^\s*profit_npoly\s*=\s*$pint/){
        $profit_npoly = $1;
    }elsif(/^\s*profit_sizex\s*=\s*$pfloat/){
        $profit_sizex = $1;
    }elsif(/^\s*profit_thresh\s*=\s*$pfloat/){
        $profit_thresh = $1;
    }elsif(/^\s*profit_nslow\s*=\s*$pint/){
        $profit_nslow = $1;
    }elsif(/^\s*profit_nblock\s*=\s*$pint/){
        $profit_nblock = $1;
    }elsif(/^\s*profit_nmed\s*=\s*$pint/){
	$profit_nmed = $1;
    }elsif(/^\s*profit_badval\s*=\s*$pfloat/){
        $profit_badval = $1;
    }elsif(/^\s*nearflat\s*=\s*(true|false)/){
	$nearflat = $1;
    }elsif(/^\s*twilight\s*=\s*$string/){
        $twilight = $1;
    }elsif(/^\s*maskfile\s*=\s*$string/){
        $maskfile = $1;
    }elsif(/^\s*flat_pos_tolerance\s*=\s*$pfloat/){
	$flat_pos_tol = $1;
    }elsif(/^\s*flat_pa_tolerance\s*=\s*$pfloat/){
	$flat_pa_tol = $1;
    }elsif(/^\s*flat_time_tolerance\s*=\s*$pfloat/){
	$flat_time_tol = $1;
    }elsif(/^\s*flat_npoly\s*=\s*$pint/){
	$flat_npoly = $1;
    }elsif(/^\s*flat_thrlo\s*=\s*$nfloat/){
	$flat_thrlo = $1;
    }elsif(/^\s*flat_thrhi\s*=\s*$pfloat/){
	$flat_thrhi = $1;
    }elsif(/^\s*flat_ncycle\s*=\s*$pint/){
	$flat_ncycle = $1;
    }elsif(/^\s*dichroic_ylo\s*=\s*$pfloat/){
        $dichroic_ylo = $1;
    }elsif(/^\s*dichroic_yhi\s*=\s*$pfloat/){
        $dichroic_yhi = $1;
    }elsif(/^\s*dichroic_balance\s*=\s*$string/){
        $dichroic_balance = $1;
    }elsif(/^\s*skymov_slo\s*=\s*$pint/){
	$skymov_slo = $1;
    }elsif(/^\s*skymov_shi\s*=\s*$pint/){
	$skymov_shi = $1;
    }elsif(/^\s*skymov_fwhm\s*=\s*$pfloat/){
	$skymov_fwhm = $1;
    }elsif(/^\s*skymov_xpos\s*=\s*$pfloat/){
	$skymov_xpos = $1;
    }elsif(/^\s*skymov_npoly\s*=\s*$pint/){
	$skymov_npoly = $1;
    }elsif(/^\s*skymov_tlo\s*=\s*$nfloat/){
	$skymov_tlo = $1;
    }elsif(/^\s*skymov_thi\s*=\s*$pfloat/){
	$skymov_thi = $1;
    }elsif(/^\s*skymov_nmin\s*=\s*$pint/){
	$skymov_nmin = $1;
    }elsif(/^\s*skymov_nwidth\s*=\s*$pint/){
	$skymov_nwidth = $1;
    }elsif(/^\s*sky_mask\s*=\s*$string/){
	$sky_mask = $1;
    }elsif(/^\s*skyfit_npoly\s*=\s*$pint/){
	$skyfit_npoly = $1;
    }elsif(/^\s*skyfit_thresh\s*=\s*$pfloat/){
	$skyfit_thresh = $1;
    }elsif(/^\s*extopt_npoly\s*=\s*$pint/){
	$extopt_npoly = $1;
    }elsif(/^\s*extopt_thresh\s*=\s*$pfloat/){
	$extopt_thresh = $1;
    }elsif(/^\s*extopt_nblock\s*=\s*$pint/){
	$extopt_nblock = $1;
    }elsif(/^\s*extopt_nmed\s*=\s*$pint/){
	$extopt_nmed = $1;
    }elsif(/^\s*extopt_ratlim\s*=\s*$pfloat/){
	$extopt_ratlim = $1;
    }elsif(/^\s*extopt_eps\s*=\s*$pfloat/){
	$extopt_eps = $1;
    }elsif(/^\s*optext_ratlim\s*=\s*$pfloat/){
	$optext_ratlim = $1;
    }elsif(/^\s*optext_eps\s*=\s*$pfloat/){
	$optext_eps = $1;
    }elsif(/^\s*optext_iave\s*=\s*$pint/){
	$optext_iave = $1;
    }elsif(/^\s*idtype_track\s*=\s*$string/){
	$idtype_track = $1;
    }elsif(/^\s*idtype_nxwidth\s*=\s*$opint/){
        $idtype_nxwidth = $1;
    }elsif(/^\s*idtype_nywidth\s*=\s*$opint/){
        $idtype_nywidth = $1;
    }elsif(/^\s*idtype_tobjm\s*=\s*$pfloat/){
	$idtype_tobjm = $1;
    }elsif(/^\s*idtype_tobja\s*=\s*$pfloat/){
	$idtype_tobja = $1;
    }elsif(/^\s*idtype_tlinm\s*=\s*$pfloat/){
	$idtype_tlinm = $1;
    }elsif(/^\s*idtype_tlina\s*=\s*$pfloat/){
	$idtype_tlina = $1;
    }elsif(/^\s*idtype_flim\s*=\s*$pfloat/){
	$idtype_flim = $1;
    }
}
close(DATAFILE);

# add any extra files to the file list

while($arg = shift @extra_files){
    if($arg =~ /@(.*)/){
	my $list = $1;
	open(LIST, $list) or die "Can't open $list\n";
	while(<LIST>){
	    chop $_;
	    push @files, $_;
	}
	close(LIST);
    }elsif($arg !~ /^\s*$/){
	push @files, $arg;
    }
}

my $nfiles = scalar(@files);

$nfiles or die "No file names specified.\n";

if(scalar(@files) == 1){
    print "\n$nfiles file name specified.\n\n";
}else{
    print "\n$nfiles file names specified.\n\n";
}

# strip off trailing .sdf s
# Check that files exist, are not directories, and are readable

my $file;
foreach $file (@files){
    $file =~ s/.sdf$//;
    -e "${file}.sdf" or die "${file}.sdf does not exist!\n";
    -f "${file}.sdf" or die "${file}.sdf is not a plain file!\n";
    -r "${file}.sdf" or die "${file} is not readable!\n";
}

# Now long series of checks that all items are defined:

(defined $dversion) or die "version undefined.\n";
if($version ne $dversion){
    print "Versions of reduce and datafile do not match:\n\n";
    print "  reduce version = $version\n";
    print "datafile version = $dversion\n";
    exit;
}

(defined $balance)       or die "balance frame undefined.\n";
(defined $zero)          or die "zero frame undefined.\n";
(defined $unit)          or die "unit frame undefined.\n";
(defined $master_sky)    or die "master sky file undefined.\n";
(defined $data_types)    or die "data types sky file undefined.\n";
(defined $reduce_script) or die "reduce script file undefined.\n";
(defined $reduce_log)    or die "reduce log file undefined.\n";
(defined $arc_pos_tol)   or die "arc position tolerance undefined.\n";
(defined $arc_pa_tol)    or die "arc slit PA tolerance undefined.\n";
(defined $arc_time_tol)  or die "arc time tolerance undefined.\n";
(defined $xstart)        or die "xstart undefined.\n";
(defined $xend)          or die "xend undefined.\n";
(defined $ystart)        or die "ystart undefined.\n";
(defined $yend)          or die "yend undefined.\n";
(defined $readout or defined $bias_region) or die "Neither a readout noise or a bias region have been defined.\n";
(defined $photon)        or die "electrons/ADU undefined.\n";
(defined $ident)         or die "object identifier undefined.\n";

# idtype parameters

(defined $idtype_nxwidth) or die "idtype_nxwidth undefined.\n";
(defined $idtype_nywidth) or die "idtype_nywidth undefined.\n";
(defined $idtype_tobjm)   or die "idtype_tobjm undefined.\n";
(defined $idtype_tobja)   or die "idtype_tobja undefined.\n";
(defined $idtype_tlinm)   or die "idtype_tlinm undefined.\n";
(defined $idtype_tlina)   or die "idtype_tlina undefined.\n";
(defined $idtype_flim)    or die "idtype_flim undefined.\n";

# tracking parameters

(defined $track_nspline) or die "Tracing spline order undefined.\n";
(defined $track_npoly) or die "Tracing polynomial order undefined.\n";

my ($trace, $track_enabled);

$track_enabled = ($track_nspline && !defined $track_template) || ($track_npoly && defined $track_template);

if($track_enabled){

    print "tracking of spectra ENABLED.\n";
    $trace = "trace=TRUE track=zzz_track";
    if(defined $track_template){
	(defined $track_tweak) or die "track_tweak undefined.\n";
	if($track_tweak eq "true"){
	    (defined $track_offset)  or die "track_offset undefined.\n";
	    (defined $track_fchange) or die "track_fchange undefined.\n";
	}else{
	    (defined $track_nobj)   or die "track_nobj undefined.\n";
	    (defined $track_iobj)   or die "track_iobj undefined.\n";
	    (defined $track_iblock) or die "track_iblock undefined.\n";
	    (defined $track_ypos)   or die "track_ypos undefined.\n";
	}
    }else{
	(defined $track_nobj)   or die "track_nobj undefined.\n";
	(defined $track_iobj)   or die "track_iobj undefined.\n";
	(defined $track_iblock) or die "track_iblock undefined.\n";
	(defined $track_ypos)   or die "track_ypos undefined.\n";
    }
    (defined $track_xstart)  or die "track_xstart undefined.\n";
    (defined $track_xend)    or die "track_xend undefined.\n";
    (defined $track_width)   or die "track_width undefined.\n";
    (defined $track_esig)    or die "track_esig undefined.\n";
    (defined $track_fwhm)    or die "track_fwhm undefined.\n";
    (defined $track_clip)    or die "track_clip undefined.\n";
    (defined $track_nblock)  or die "track_nblock undefined.\n";
    (defined $track_psig)    or die "track_psig undefined.\n";
    (defined $track_tcycle)  or die "track_tcycle undefined.\n";
    (defined $track_change)  or die "track_change undefined.\n";
    (defined $profit_npoly)  or die "profit_npoly undefined.\n";
    (defined $profit_sizex)  or die "profit_sizex undefined.\n";
    (defined $profit_thresh) or die "profit_thresh undefined.\n";
    (defined $profit_nslow)  or die "profit_nslow undefined.\n";
    (defined $profit_nblock) or die "profit_nblock undefined.\n";
    (defined $profit_nmed)   or die "profit_nmed undefined.\n";
    (defined $profit_badval) or die "profit_badval undefined.\n";
    (defined $optext_ratlim) or die "optext_ratlim undefined.\n";
    (defined $optext_eps)    or die "optext_eps undefined.\n";
    (defined $optext_iave)   or die "optext_iave undefined.\n";

}else{

    print "tracking of spectra DISABLED.\n";
    $trace = "trace=FALSE";
    (defined $extopt_npoly)  or die "extopt_npoly undefined.\n";
    (defined $extopt_thresh) or die "extopt_thresh undefined.\n";
    (defined $extopt_nblock) or die "extopt_nblock undefined.\n";
    (defined $extopt_nmed)   or die "extopt_nmed undefined.\n";
    (defined $extopt_ratlim) or die "extopt_ratlim undefined.\n";
    (defined $extopt_eps)    or die "extopt_eps undefined.\n";
}

# flat parameters

(defined $nearflat) or die "nearflat undefined.\n";
if($nearflat eq "true"){
    (defined $twilight)      or die "twilight frame undefined.\n";
    (defined $maskfile)      or die "maskfile file undefined.\n";
    (defined $flat_pos_tol)  or die "flat position tolerance undefined.\n";
    (defined $flat_pa_tol)   or die "flat slit PA tolerance undefined.\n";
    (defined $flat_time_tol) or die "flat time tolerance undefined.\n";
    (defined $flat_npoly  )  or die "flat_npoly  undefined.\n";
    (defined $flat_thrlo  )  or die "flat_thrlo  undefined.\n";
    (defined $flat_thrhi  )  or die "flat_thrhi  undefined.\n";
    (defined $flat_ncycle )  or die "flat_ncycle undefined.\n";
    (defined $dichroic_ylo ) or die "dichroic_ylo undefined.\n";
    if($dichroic_ylo > 0.0){
	(defined $dichroic_yhi )     or die "dichroic_yhi undefined.\n";
	(defined $dichroic_balance ) or die "dichroic_balance undefined.\n";
    }
}

(defined $skymov_slo)    or die "skymov_slo undefined.\n";
(defined $skymov_shi)    or die "skymov_shi undefined.\n";
(defined $skymov_fwhm)   or die "skymov_fwhm undefined.\n";
(defined $skymov_xpos)   or die "skymov_xpos undefined.\n";
(defined $skymov_npoly)  or die "skymov_npoly undefined.\n";
(defined $skymov_tlo)    or die "skymov_tlo undefined.\n";
(defined $skymov_thi)    or die "skymov_thi undefined.\n";
(defined $skymov_nmin)   or die "skymov_nmin undefined.\n";
(defined $skymov_nwidth) or die "skymov_nwidth undefined.\n";

if(defined $sky_mask){
    -e "$sky_mask" or die "Failed to find sky mask file = $sky_mask\n";
}

(defined $skyfit_npoly) or die "skyfit_npoly undefined.\n";
(defined $skyfit_thresh) or die "skyfit_thresh undefined.\n";

# Check existence of files

-e "$balance.sdf"    or die "Failed to find balance frame = $balance.sdf\n";
-e "$zero.sdf"       or die "Failed to find zero frame = $zero.sdf\n";
-e "$unit.sdf"       or die "Failed to find unit frame = $unit.sdf\n";
-e "$master_sky.sdf" or die "Failed to find master sky file = $master_sky.sdf\n";

if(defined $idtype_track){
    -e "$idtype_track.sdf" or die "Failed to find idtype track file = $idtype_track.sdf\n";
}

if($skip){
    -e $data_types or die "Failed to find data types files as needed for option -s\n";
}

(defined $readout or -e $bias_region) or die "Failed to find bias region file = $bias_region\n";

if($nearflat eq "true"){
    -e "$twilight.sdf" or die "Failed to find twilight frame = $twilight.sdf\n";
    -e $maskfile       or die "Failed to find ARD maskfile = $maskfile\n";
    if($dichroic_ylo > 0.0){
	-e "$dichroic_balance.sdf" or die "Failed to find dichroic balance = $dichroic_balance.sdf\n";
    }
}

# A few sanity checks
($xstart > 0 && ($xstart <= $xend)) or die "X range: $xstart to $xend is invalid.\n";
($ystart > 0 && ($ystart <= $yend)) or die "Y range: $ystart to $yend is invalid.\n";
($track_xstart > 0 && ($track_xstart <= $track_xend)) or die "Track X range: $track_xstart to $track_xend is invalid.\n";
($photon > 0.) or die "Electrons/ADU (=$photon) must be > 0\n";
($skymov_slo > 0 && ($skymov_slo <= $skymov_shi)) or die "Y range: $ystart to $yend is invalid.\n";
$skymov_npoly  or die "skymov_npoly (=$skymov_npoly) must be > 0\n";
$extopt_npoly  or die "extopt_npoly (=$extopt_npoly) must be > 0\n";
$extopt_nblock or die "extopt_nblock (=$extopt_nblock) must be > 0\n";

# Number of days at start of each month from beinning of year.
# Leap years are accounted for later.

my @months = (0,31,59,90,120,151,181,212,243,273,304,334);


# Finally, we have inputs and they have checked out.

# Generate script for extracting header information and for typing
# the data files. Each file is assumed to have the relevant headers
# gathered in a structure called .more.pamela

open(JUNK, ">zzz_header_info") or die "Can't open zzz_header_info\n";

print JUNK "#!/bin/csh\n";
print JUNK "#\n\n";
print JUNK "source \$STARLINK_DIR/etc/cshrc\n";
print JUNK "source \$STARLINK_DIR/etc/login\n";
print JUNK "pamela\n";
print JUNK "set verbose\n";

foreach $file (@files){
    if($file =~ /\S/){
	print JUNK "\necho File name = $file\n";
	print JUNK "\nhdstrace $file.more.pamela\n\n";

	if(!$skip){

	    if(defined $idtype_track){
		print JUNK
		    "idtype image=$file flat=$balance trace=true track=$idtype_track "
		    . "xstart=$xstart xend=$xend ystart=$ystart yend=$yend "
		    . "nxwidth=$idtype_nxwidth nywidth=$idtype_nywidth "
		    . "tobjm=$idtype_tobjm tobja=$idtype_tobja tlinm=$idtype_tlinm "
		    . "tlina=$idtype_tlina flim=$idtype_flim\n";
	    }else{
		print JUNK
		    "idtype image=$file flat=$balance trace=false "
		    . "xstart=$xstart xend=$xend ystart=$ystart yend=$yend "
		    . "nxwidth=$idtype_nxwidth nywidth=$idtype_nywidth "
		    . "tobjm=$idtype_tobjm tobja=$idtype_tobja tlinm=$idtype_tlinm "
		    . "tlina=$idtype_tlina flim=$idtype_flim\n";
	    }
	}
    }
}

close(JUNK);

if($skip){
    print "Extracting header info only (data types assumed pre-determined) ...\n\n";
}else{
    print "Extracting header info and data typing ...\n\n";
}

system("chmod +x zzz_header_info; ./zzz_header_info > zzz_header_info.log 2>&1");

if($skip){
    print "Finished extracting header info.\n";
}else{
    print "Finished extracting header info and data typing.\n";
}

print "Script: zzz_header  ---> zzz_header_info.log.\n\n";

# zzz_header_info.log contains extracted header info and (optionally) the data
# types from idtype. Now dig this out

open(HEADER, 'zzz_header_info.log') or die "Can't open zzz_header_info.log\n";

my (%ra, %dec, %day, %month, %year, %utc, %slitpa, %jd, %source);
while(<HEADER>){

    if(/^File name = (\S*)/){
	$file = $1;
    }

    if(/^\s*RA\s*<_DOUBLE>\s*(\S*)/){
	$ra{$file}  = $1;
    }elsif(/^\s*DEC\s*<_DOUBLE>\s*(\S*)/){
	$dec{$file} = $1;
    }elsif(/^\s*JD\s*<_DOUBLE>\s*(\S*)/){
	$jd{$file}  = $1;
    }elsif(/^\s*SLITPA\s*<_REAL>\s*(\S*)/){
	$slitpa{$file}  = $1;
    }elsif(/^\s*DATE\s*<_CHAR\*10>\s*\'(\d\d)\/(\d\d)\/(\d\d\d\d)\'/){
	$day{$file}   = $1;
	$month{$file} = $2;
	$year{$file}  = $3;
    }elsif(/^\s*UTC\s*<_CHAR\*12>\s*\'(\d\d):(\d\d):(\d\d.\d\d\d)\'/){
	$utc{$file}   = $1  + $2/60 + $3/3600;
    }elsif(!$skip && /^Type = (\S*), total/){
	$source{$file} = $1;
    }

}
close(HEADER);

# If the data typing is skipped, they must be loaded in from a data file

if($skip){
    my ($v1,$v2,$v3,$v4);
    open(TYPES, $data_types) or die "Could not open data types file = $data_types for reading\n";
    while(<TYPES>){
	($v1,$v2,$v3,$v4) = split;
	$v2 =~ s/,$//;
	$source{$v2} = $v4;
    }
}

# Check that all header items were found and compute day time

my %time;

foreach $file (@files){
    if($file =~ /\S/){

# We must always know the data type

	(defined $source{$file}) or die "source undefined for file = $file\n";

# If we know these headers we can select the arcs with greater confidence,
# but we can also

	(defined $ra{$file})     or print "WARNING: ra undefined for $file. Could cause problems with arc/object association.\n";
	(defined $dec{$file})    or print "WARNING: dec undefined for $file. Could cause problems with arc/object association.\n";
	(defined $slitpa{$file}) or print "WARNING: slit PA undefined for $file. Could cause problems with arc/object association.\n";

	if(defined $jd{$file}){

	    $time{$file} = $jd{$file};

	}elsif(defined $year{$file} && defined $month{$file} && defined $day{$file} && defined $utc{$file}){

# Compute time in days from zero-point of start of 1980.

	    $year{$file} > 1979 or die "Can only cope with years > 1979\n";
	    my $offset = 0;
	    for(my $n=1980; $n<$year{$file}; $n++){
		if($n % 4 == 0){
		    $offset += 366;
		}else{
		    $offset += 365;
		}
	    }
	    $time{$file} = $offset + $months[$month{$file}-1] + $day{$file} - 1 + $utc{$file}/24.;
	    if($year{$file} == 4*int($year{$file}/4) && $month{$file} > 2){
		$time{$file}++;
	    }

	}else{
	    print "WARNING: could not find either JD or UTC & DATE in .more.pamela in file = $file. Could cause problems with arc/object association.\n";
	}
    }
}

# Dump a file of data types

if(!$skip){

    open(TYPES, ">$data_types") or die "Failed to open $data_types\n";
    foreach $file (@files){
	print TYPES "File: $file, id: $source{$file}\n";
    }
    close(TYPES);
    print "Identified data types of files written to $data_types\n\n";

    if($types){
	print "\n-t option specified, so no reduction will be carried out.\n";
	exit;
    }
}


# Look for all arcs and flats associated with every object.

my $twopi = 8.*atan2(1.,1.);

my ($slew, $rdiff, $ddiff, $cosd, $pdiff, $padiff, $tdiff, $ok, $nbefore, $nafter);
my (%arcs, %flats_before, %flats_after, $obj, $arc, $narc, $flat);

foreach $obj (@files){
    if($obj =~ /\S/ && $source{$obj} eq 'DATA'){
        $narc = 0;
        foreach $arc (@files){
            if($arc =~ /\S/ && $source{$arc} eq 'ARC'){

                if(defined $ra{$arc} && defined $ra{$obj}){
                    $rdiff = $ra{$arc}-$ra{$obj};
                }else{
                    $rdiff = 0.;
                }

                if(defined $dec{$arc} && defined $dec{$obj}){
                    $ddiff = $dec{$arc}-$dec{$obj};
                    $cosd  = cos($twopi*$dec{$obj}/360.);
                    $pdiff = sqrt(($rdiff*$cosd)**2 + $ddiff**2);
                }else{
                    $ddiff = 0.;
                    $pdiff = 0.;
                }

                if(defined $slitpa{$arc} && defined $slitpa{$obj}){
                    $padiff = abs($slitpa{$arc}-$slitpa{$obj});
                }else{
                    $padiff = 0.;
                }

                if(defined $time{$arc} && defined $time{$obj}){
                    $tdiff = 24.*60.*abs($time{$arc}-$time{$obj});
                }else{
                    $tdiff = 0.;
                }

                if($arc_pos_tol <= 0. || ($pdiff < $arc_pos_tol && $padiff < $arc_pa_tol && $tdiff < $arc_time_tol)){

                    # We now have a possible arc but we now check that there are no
                    # exposures between it (in time) and the object at a different position
                    # and or slit PA (override if arc_pos_tolerance <= 0)

                    $ok = 1;
                    if($arc_pos_tol > 0.){
                        foreach $slew (@files){
                            if($slew =~ /\S/){
                                if(defined $time{$slew} && defined $time{$arc} && defined $time{obj} &&
                                   (($time{$slew} > $time{$arc} && $time{$slew} < $time{$obj}) ||
                                    ($time{$slew} < $time{$arc} && $time{$slew} > $time{$obj}))){
                                    $rdiff = $ra{$slew}-$ra{$obj};
                                    $ddiff = $dec{$slew}-$dec{$obj};
                                    $pdiff = sqrt(($rdiff*$cosd)**2 + $ddiff**2);
                                    $padiff= abs($slitpa{$arc}-$slitpa{$obj});
                                    if($pdiff > $arc_pos_tol || $padiff > $arc_pa_tol){
                                        $ok = 0;
                                    }
                                }
                            }
                        }
                    }
                    # add to list of arcs
                    if($ok){
                        $arcs{$obj}[$narc++] = $arc;
                    }
                }
            }
        }

	if($nearflat eq "true"){
	    $nbefore = $nafter = 0;
	    foreach $flat (@files){
		if($flat =~ /\S/ && $source{$flat} eq 'FLAT'){

		    if(defined $ra{$flat} && defined $ra{$obj}){
			$rdiff = $ra{$flat}-$ra{$obj};
		    }else{
			$rdiff = 0.;
		    }

		    if(defined $dec{$flat} && defined $dec{$obj}){
			$ddiff = $dec{$flat}-$dec{$obj};
			$cosd  = cos($twopi*$dec{$obj}/360.);
			$pdiff = sqrt(($rdiff*$cosd)**2 + $ddiff**2);
		    }else{
			$ddiff = 0.;
			$pdiff = 0.;
		    }

		    if(defined $slitpa{$flat} && defined $slitpa{$obj}){
			$padiff = abs($slitpa{$flat}-$slitpa{$obj});
		    }else{
			$padiff = 0.;
		    }

		    if(defined $time{$flat} && defined $time{$obj}){
			$tdiff = 24.*60.*abs($time{$flat}-$time{$obj});
		    }else{
			$tdiff = 0.;
		    }

		    if($pdiff < $flat_pos_tol && $padiff < $flat_pa_tol && abs($tdiff) < $flat_time_tol){

# We now have a possible flat but we now check that there are no
# exposures between it (in time) and the object at a different position
# and or slit PA

			$ok = 1;
			foreach $slew (@files){
			    if($slew =~ /\S/){
				if(defined $time{$slew} && defined $time{$flat} && defined $time{obj} &&
				   (($time{$slew} > $time{$flat} && $time{$slew} < $time{$obj}) ||
				    ($time{$slew} < $time{$flat} && $time{$slew} > $time{$obj}))){
				    $rdiff = $ra{$slew}-$ra{$obj};
				    $ddiff = $dec{$slew}-$dec{$obj};
				    $pdiff = sqrt(($rdiff*$cosd)**2 + $ddiff**2);
				    $padiff= abs($slitpa{$flat}-$slitpa{$obj});
				    if($pdiff > $flat_pos_tol || $padiff > $flat_pa_tol){
					$ok = 0;
				    }
				}
			    }
			}
			if($ok){
			    if($tdiff < 0.){
				$flats_before{$obj}[$nbefore++] = $flat;
			    }else{
				$flats_after{$obj}[$nafter++] = $flat;
			    }
			}
		    }
		}
	    }
	}
    }
}


# OK, now we have finally have found all arcs and flats associated with every object,
# we can generate the reduction script.

open(REDUCE, ">$reduce_script") or die "Can't open $reduce_script\n";

print REDUCE "#!/bin/csh\n\n";
print REDUCE "#\n";
print REDUCE "# This script was generated automatically using reduce.\n";
print REDUCE "#\n\n";

print REDUCE "source \$STARLINK_DIR/etc/cshrc\n";
print REDUCE "source \$STARLINK_DIR/etc/login\n";
print REDUCE "figaro > /dev/null\n";
print REDUCE "kappa  > /dev/null\n";
print REDUCE "pamela\n";
print REDUCE "set verbose\n";

# Now wind through each object
my ($bframe, $tdiff_before, $tdiff_after, $w_before, $w_after, $rnorm);
foreach $obj (@files){
    if($obj =~ /\S/ && $source{$obj} eq 'DATA'){

	print REDUCE "echo Starting file = $obj\n";

# determine readout noise from bias region

	if(defined $bias_region){
	    print REDUCE "set read = `picstat $obj $bias_region 3.5 noplot | grep 'Revised standard' | sed -e 's/Revised.*= *//'`\n";
	}else{
	    print REDUCE "set read = $readout\n";
	}

	print REDUCE "echo \"Readout noise = \$read\"\n";

	if($nearflat eq "true"){

	    if(@{$flats_before{$obj}} || @{$flats_after{$obj}}){

# Next prepare before and after flats along with their mean times before
# and after.

# Prepare flat before (goes to zzz_before if at least 1 flat)

		if (@{$flats_before{$obj}} == 1) {
		    print REDUCE "cp $flats_before{$obj}[0].sdf zzz_before.sdf \n";
		    $tdiff_before = $time{$flats_before{$obj}[0]}-$time{$obj};

		}elsif(@{$flats_before{$obj}} > 1) {
		    print REDUCE "rm zzz_before.lis\n";
		    print REDUCE "touch zzz_before.lis\n";

		    $tdiff_before = 0;
		    foreach $flat (@{$flats_before{$obj}}){
			print REDUCE "echo $flat >> zzz_before.lis\n";
			$tdiff_before += $time{$flat}-$time{$obj};
		    }
		    $tdiff_before /= @{$flats_before{$obj}};
		    print REDUCE "medsky files=zzz_before.lis scaled=true output=zzz_before\n";
		}

# Prepare flat after (goes to zzz_after if at least 1 flat)

		if (@{$flats_after{$obj}} == 1) {
		    print REDUCE "cp $flats_after{$obj}[0].sdf zzz_after.sdf \n";
		    $tdiff_after = $time{$flats_after{$obj}[0]}-$time{$obj};

		}elsif(@{$flats_after{$obj}} > 1) {
		    print REDUCE "rm zzz_after.lis\n";
		    print REDUCE "touch zzz_after.lis\n";

		    $tdiff_after = 0;
		    foreach $flat (@{$flats_after{$obj}}){
			print REDUCE "echo $flat >> zzz_after.lis\n";
			$tdiff_after += $time{$flat}-$time{$obj};
		    }
		    $tdiff_after /= scalar(@{$flats_after{$obj}});
		    print REDUCE "medsky files=zzz_after.lis scaled=true output=zzz_after\n";
		}

# Linearly interpolate in time between before and after flats.

		if (@{$flats_before{$obj}} && @{$flats_after{$obj}} ){

		    $w_before =  $tdiff_after/($tdiff_after - $tdiff_before);
		    $w_after  = -$tdiff_before/($tdiff_after - $tdiff_before);

		    print REDUCE "icmult image=zzz_before factor=$w_before output=zzz_before\n";
		    print REDUCE "icmult image=zzz_after  factor=$w_after  output=zzz_after\n";
		    print REDUCE "iadd image=zzz_before image1=zzz_after output=zzz_flat\n";

		}elsif(@{$flats_before{$obj}}){

		    print REDUCE "cp zzz_before.sdf zzz_flat.sdf \n";

		}elsif(@{$flats_after{$obj}}){

		    print REDUCE "cp zzz_after.sdf zzz_flat.sdf \n";
		}

# The flat is called = zzz_flat. Now do the usual manipulations to get to a balance frame,
# which extraction of mean spectrum, masking bad regions, taking log, fitting, anti-logging,
# dividing out fit to spectrum, replacing spatial profile with one from twilight, making
# correction for dichroic ripple (in weak flat cases), normalisation by the mean and clipping
# extreme values.

		print REDUCE "ystract image=zzz_flat xstart=$xstart xend=$xend spectrum=zzz_ycut\n";
		print REDUCE "ardmask in=zzz_ycut ardfile=$maskfile out=zzz_ycutmasked \n";
		print REDUCE "logar base=10 in=zzz_ycutmasked out=zzz_lycutmasked\n" ;
		print REDUCE "polfit input=zzz_lycutmasked output=zzz_lyfit npoly=$flat_npoly "
		    ." thrlo=$flat_thrlo thrhi=$flat_thrhi ncycle=$flat_ncycle plot=no\n";
		print REDUCE "expon base=10 in=zzz_lyfit out=zzz_yfit\n " ;
		print REDUCE "isydiv zzz_flat zzz_yfit zzz_flat \n";
		print REDUCE "extract image=zzz_flat ystart=$ystart yend=$yend spectrum=zzz_flat_prof \n";
		print REDUCE "isxdiv image=zzz_flat spectrum=zzz_flat_prof output=zzz_flat_isxdiv \n";
		print REDUCE "extract image=$twilight ystart=$ystart yend=$yend spectrum=zzz_twilight_prof \n";
		print REDUCE "isxmul image=zzz_flat_isxdiv spectrum=zzz_twilight_prof output=zzz_flat\n";
		print REDUCE "idiv unit zzz_flat zzz_tonka \n";

		if ($dichroic_ylo > 0.0) {
		    $rnorm = 1 + $dichroic_yhi - $dichroic_ylo ;
		    print REDUCE "cp -f zzz_tonka.sdf zzz_save_tonka.sdf\n";
		    print REDUCE "ystract image=zzz_tonka xstart=$dichroic_ylo"
			. " xend=$dichroic_yhi spectrum=zzz_ripple \n";
		    print REDUCE "icdiv zzz_ripple $rnorm zzz_ripple \n";
		    print REDUCE "isymul $dichroic_balance zzz_ripple zzz_tonka \n";

		}

# Normalise by the mean

		print REDUCE "istat image=zzz_tonka ystart=$ystart yend=$yend"
		    ." xstart=$xstart xend=$xend\n";
		print REDUCE "icdiv image=zzz_tonka factor=@\$ADAM_USER/GLOBAL.STAT_MEAN output=zzz_tonka\n";
		print REDUCE "clip zzz_tonka 0.01 100.0 zzz_tonka\n";

		$bframe = "zzz_tonka";
	    }else{

		print REDUCE "echo No flats associated with $obj. Using default instead\n";

		$bframe = $balance;
	    }

	}else{

# Just use standard balance frame.

	    $bframe = $balance;

	}

# Tracing

	if($track_enabled){

	    if(defined $track_template){

		print REDUCE "\n\\cp -f $track_template.sdf zzz_track.sdf\n";
		if($track_tweak eq "true"){
		    print REDUCE
			"\ntrack image=$obj flat=$bframe old=true track=zzz_track nspline=$track_nspline "
			. "npoly=$track_npoly nfpoly=1 xstart=$track_xstart xend=$track_xend "
			. "ystart=$ystart yend=$yend tweak=true offset=$track_offset fchange=$track_fchange "
			. "plot=no readout=\$read width=$track_width esig=$track_esig "
			. "fwhm=$track_fwhm change=$track_change photon=$photon "
			. "clip=$track_clip nblock=$track_nblock tcycle=$track_tcycle "
			. " psig=$track_psig\n\n";
		}else{
		    print REDUCE
			"\ntrack image=$obj flat=$bframe old=true track=zzz_track nspline=$track_nspline "
			. "npoly=$track_npoly nfpoly=1 xstart=$track_xstart xend=$track_xend "
			. "ystart=$ystart yend=$yend tweak=false pick=true nobj=$track_nobj "
			. "iobj=$track_iobj iblock=$track_iblock ypos=$track_ypos "
			. "plot=no readout=\$read width=$track_width esig=$track_esig "
			. "fwhm=$track_fwhm change=$track_change photon=$photon "
			. "clip=$track_clip nblock=$track_nblock tcycle=$track_tcycle "
			. " psig=$track_psig\n\n";
		}
	    }else{
		print REDUCE
		    "\ntrack image=$obj flat=$bframe old=false track=zzz_track nspline=$track_nspline "
		    . "npoly=$track_npoly nfpoly=1 xstart=$track_xstart xend=$track_xend "
		    . "ystart=$ystart yend=$yend pick=true nobj=$track_nobj "
		    . "iobj=$track_iobj iblock=$track_iblock ypos=$track_ypos "
		    . "plot=no readout=\$read width=$track_width esig=$track_esig "
		    . "fwhm=$track_fwhm change=$track_change photon=$photon "
		    . "clip=$track_clip nblock=$track_nblock tcycle=$track_tcycle "
		    . " psig=$track_psig\n\n";

	    }
	}

#
# Now move sky regions
#
	print REDUCE
	    "\nskymov image=$obj flat=$bframe region=$master_sky "
	    . "$trace outsky=zzz_sregion xstart=$xstart xend=$xend "
	    . "ystart=$ystart yend=$yend slo=$skymov_slo shi=$skymov_shi "
	    . "fwhm=$skymov_fwhm xpos=$skymov_xpos npoly=$skymov_npoly "
	    . "thrlo=$skymov_tlo thrhi=$skymov_thi nmin=$skymov_nmin median=yes "
	    . "nwidth=$skymov_nwidth\n\n";

#
# Optionally mask bad pixels
#

	if(defined $sky_mask){
	    print REDUCE "\nardmask in=zzz_sregion ardfile=$sky_mask out=zzz_junk\n\n";
	    print REDUCE "\nmv zzz_junk.sdf zzz_sregion.sdf\n\n";
	}

#
# Fit polynomials to sky
#

	print REDUCE
	    "\nskyfit image=$obj flat=$bframe dload=no region=zzz_sregion "
	    . "$trace sky=zzz_skfit xstart=$xstart xend=$xend ystart=$ystart "
	    . "yend=$yend npoly=$skyfit_npoly thresh=$skyfit_thresh "
	    . "readout=\$read photon=$photon\n\n";

#
# Normal extraction
#

	print REDUCE
	    "\nextnor image=$obj flat=$bframe dload=no region=zzz_sregion "
	    . "sky=zzz_skfit $trace spect=${obj}nor$ident ystart=$ystart "
	    . "yend=$yend readout=\$read photon=$photon\n\n";

#
# Optimal extraction
#

	if($track_enabled){

	    print REDUCE
		"\nprofit image=$obj flat=$bframe dload=no region=zzz_sregion "
		."sky=zzz_skfit track=zzz_track fract=zzz_fract ystart=$ystart yend=$yend "
		. "readout=\$read photon=$photon npoly=$profit_npoly "
		. "sizex=$profit_sizex thresh=$profit_thresh nslow=$profit_nslow badval=$profit_badval "
		. "nblock=$profit_nblock nmed=$profit_nmed plot=no\n";

	    print REDUCE
		"\noptext image=$obj flat=$bframe dload=no region=zzz_sregion "
		. "sky=zzz_skfit track=zzz_track fract=zzz_fract spect=${obj}opt$ident "
		. "ystart=$ystart yend=$yend readout=\$read photon=$photon "
		. "zaprats=true ratlo=-$optext_ratlim rathi=$optext_ratlim "
		. "eps=$optext_eps plot=no iave=$optext_iave \n";

	}else{

	    print REDUCE
		"\nextopt image=$obj flat=$bframe region=zzz_sregion sky=zzz_skfit "
		. "spect=${obj}opt$ident ystart=$ystart yend=$yend readout=\$read "
		. "photon=$photon old=no npoly=$extopt_npoly profit=zzz_profile "
		. "thresh=$extopt_thresh nblock=$extopt_nblock nmed=$extopt_nmed zaprats=yes dload=no "
		. "ratlo=-$extopt_ratlim rathi=$extopt_ratlim eps=$extopt_eps "
		. "plot=no\n";

	}

#
# Extraction of sky spectra too. Normal first
#

	print REDUCE
	    "\nextnor image=zzz_skfit flat=$unit region=zzz_sregion sky=$zero "
	    . "$trace ystart=$ystart yend=$yend dload=no "
	    . "spect=${obj}skyn$ident readout=\$read photon=$photon\n\n";

#
# Then optimal
#

	if($track_enabled){

	    print REDUCE
		"\noptext image=zzz_skfit flat=$unit region=zzz_sregion sky=$zero "
		. "track=zzz_track fract=zzz_fract spect=${obj}skyo$ident dload=no "
		. "ystart=$ystart yend=$yend readout=\$read photon=$photon "
		. "zaprats=no plot=no iave=$optext_iave\n\n";

	}else{

	    print REDUCE
		"\nextopt image=zzz_skfit flat=$unit region=zzz_sregion sky=$zero "
		. "spect=${obj}skyo$ident ystart=$ystart yend=$yend readout=\$read "
		. "photon=$photon old=yes profit=zzz_profile zaprats=no dload=no "
		. "plot=no\n\n";

	}

#
# Now for the associated arcs (if any)
#

	foreach $arc (@{$arcs{$obj}}){

#
# Normal first
#

	    print REDUCE
		"\nextnor image=$arc flat=$bframe region=zzz_sregion sky=$zero "
		. "$trace spect=$arc${obj}arcn$ident ystart=$ystart dload=no "
		. "yend=$yend readout=\$read photon=$photon\n\n";

#
# Then optimal
#

	    if($track_enabled){

		print REDUCE
		    "\noptext image=$arc flat=$bframe region=zzz_sregion "
		    . "sky=$zero track=zzz_track fract=zzz_fract dload=no "
		    . "spect=$arc${obj}arco$ident ystart=$ystart yend=$yend "
		    . "readout=\$read photon=$photon zaprats=no plot=no "
		    . "iave=$optext_iave\n\n";

	    }else{

		print REDUCE
		    "\nextopt image=$arc flat=$bframe region=zzz_sregion "
		    . "sky=$zero spect=$arc${obj}arco$ident ystart=$ystart "
		    . "yend=$yend dload=no readout=\$read photon=$photon "
		    . "old=yes profit=zzz_profile zaprats=no plot=no\n\n";

	    }
	}
    }
}
close(REDUCE);

print "Now running reduction script ...\n\n";

system("chmod +x $reduce_script; ./$reduce_script > $reduce_log 2>&1");

# Finally add stuff to the end of the reduction log.

open(LOG, ">>$reduce_log") or die "Failed to open $reduce_log\n";

print LOG "\n\nReduction summary:\n";

print LOG "\nReduce script version date = $version\n";
print LOG "Identified data types of files written to $data_types\n";
print LOG "Reduction script: $reduce_script\n";
print LOG "Command line arguments: $sargs\n\n";

print LOG "\nRegion of frame, X: $xstart to $xend, Y: $ystart to $yend\n";
if(defined $bias_region){
    print LOG "Readout noise measured from bias region file: $bias_region\n";
    print LOG "Gain, electrons/ADU: $photon\n";
}else{
    print LOG "Readout noise: $readout, gain, electrons/ADU: $photon\n";
}

print LOG "\nExternal files used:\n\n";

if($balance !~ "0"){
    print LOG "                     Balance frame used: $balance\n";
}
print LOG "                        Zero frame used: $zero\n";
print LOG "                        Unit frame used: $unit\n";
print LOG "                        Master sky file: $master_sky\n";

print LOG "             Position tolerance on arcs: $arc_pos_tol degrees\n";
print LOG "              Slit PA tolerance on arcs: $arc_pa_tol degrees\n";
print LOG "                 Time tolerance on arcs: $arc_time_tol minutes\n";

# idtype parameters

print LOG "\nidtype parameters:\n\n";
if(defined $idtype_track){
    print LOG "                        Track file used: $idtype_track\n";
}else{
    print LOG "No track file used.\n";
}
print LOG "        Multiplicative object threshold: $idtype_tobjm\n";
print LOG "              Additive object threshold: $idtype_tobja\n";
print LOG "          Multiplicative line threshold: $idtype_tlinm\n";
print LOG "                Additive line threshold: $idtype_tlina\n";
print LOG "               Flat/junk dividing limit: $idtype_flim\n";

if($track_enabled){
    print LOG "\ntrack parameters:\n\n";
    if(defined $track_template){
	print LOG "         Track template file used, name: $track_template\n";
    }else{
	print LOG "No Track template file used\n";
    }
    print LOG "          Number of spline coefficients: $track_nspline\n";
    print LOG "            Number of poly coefficients: $track_npoly\n";
    print LOG "              Number of objects on slit: $track_nobj\n";
    print LOG "                  Which object to track: $track_iobj\n";
    print LOG "        Number of rows to locate object: $track_iblock\n";
    print LOG "                  Width of track window: $track_width\n";
    print LOG "                     Initial y position: $track_ypos\n";
    print LOG " FWHM of gaussian for cross-correlation: $track_esig\n";
    print LOG "                 Estimated profile FWHM: $track_fwhm\n";
    print LOG "       Sigma clip threshold during fits: $track_clip\n";
    print LOG "        Block size for tracking spectra: $track_nblock\n";
    print LOG "   Sigma clip threshold during blocking: $track_psig\n";
    print LOG " Number of rejection cycles during fits: $track_tcycle\n";
    print LOG "  Pixel limit for rejection during fits: $track_change\n";
}else{
    print LOG "\nNo tracking used.\n\n";
}

print LOG "\nskymov parameters:\n\n";
print LOG "                 Start of search region: $skymov_slo\n";
print LOG "                   End of search region: $skymov_shi\n";
print LOG "               FWHM for object location: $skymov_fwhm\n";
print LOG "              Object reference position: $skymov_xpos\n";
print LOG "            Number of poly coefficients: $skymov_npoly\n";
print LOG "           Lower sigma reject threshold: $skymov_tlo\n";
print LOG "           Upper sigma reject threshold: $skymov_thi\n";
print LOG "                Minimum bad pixel block: $skymov_nmin\n";
print LOG "                    Median filter width: $skymov_nwidth\n";

if(defined $sky_mask){
    print LOG "                   Sky bad pixel mask: $sky_mask\n";
}else{
    print LOG " No sky bad pixel mask used\n";
}

print LOG "\nskyfit parameters:\n\n";
print LOG "            Number of poly coefficients: $skyfit_npoly\n";
print LOG "                 Sigma reject threshold: $skyfit_thresh\n";

if($track_enabled){
    print LOG "\nprofit parameters:\n\n";
    print LOG " Number of coefficients for column fits: $profit_npoly\n";
    print LOG "   Pixel separation between polynomials: $profit_sizex\n";
    print LOG "     Sigma clip threshold for poly fits: $profit_thresh\n";
    print LOG "          Max pixel-by-pixel rejections: $profit_nslow\n";
    print LOG "                    Block size for fits: $profit_nblock\n";
    print LOG "                    Median filter width: $profit_nmed\n";
    print LOG "                    Bad pixel threshold: $profit_badval\n";

    print LOG "\noptext parameters:\n\n";
    print LOG "             Cosmic ray rejection limit: $optext_ratlim\n";
    print LOG "  Factor to prevent spurious rejections: $optext_eps\n";
    print LOG "   Number of sky pixels to average over: $optext_iave\n";
}else{

    print LOG "\nextopt parameters:\n\n";
    print LOG "  Number of poly coeffs for column fits: $extopt_npoly\n";
    print LOG "                 Sigma reject threshold: $extopt_thresh\n";
    print LOG "                   Number of rows/block: $extopt_nblock\n";
    print LOG "   Cosmic ray rejection sigma threshold: $extopt_ratlim\n";
    print LOG "                    Median filter width: $extopt_nmed\n";
    print LOG "              Fraction tolerance factor: $extopt_eps\n";
}

if($nearflat eq "true"){
    print LOG "\nDynamic generation of flat fields used.\n";
    print LOG "                         Twilight frame: $twilight\n";
    print LOG "                          ARD mask file: $maskfile\n";
    print LOG "            Position tolerance on flats: $flat_pos_tol degrees\n";
    print LOG "             Slit PA tolerance on flats: $flat_pa_tol degrees\n";
    print LOG "                Time tolerance on flats: $flat_time_tol minutes\n";
    print LOG " Poly coeffs for spectral varn of flats: $flat_npoly\n";
    print LOG "   Lower reject threshold for flat fits: $flat_thrlo\n";
    print LOG "   Lower reject threshold for flat fits: $flat_thrhi\n";
    print LOG "  Number of reject cycles for flat fits: $flat_ncycle\n";

    if($dichroic_ylo != 0.){
	print LOG "\nDichroic ripple correction enabled.\n";
	print LOG "       Lower limit of dichroic response: $dichroic_ylo\n";
	print LOG "       Upper limit of dichroic response: $dichroic_yhi\n";
	print LOG "            Dichroic balance frame used: $dichroic_balance\n";
    }else{
	print LOG "\nDichroic ripple correction not enabled.\n";
    }
}else{
    print LOG "\nDynamic generation of flat fields not used.\n";
}
close(LOG);

print "Everything finished!\a\n\n";
print "Reduction script: $reduce_script, reduction log: $reduce_log\n\n\n";

exit;
