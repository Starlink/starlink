#!/usr/bin/perl
#
# !!begin
# !!title  Image rotation script
# !!author T.R.Marsh
# !!created 14 January 2001
# !!revised 08 December 2005
# !!root   rotate
# !!index  rotate.pl
# !!descr  Perl script for rotating frames by 90 degrees
# !!head1  Rotation perl script
#
# !!table
# !!trow{!!emph{Name:}}{rotate}
# !!trow{!!emph{Purpose:}}{to rotate frames to get dispersion
# into Y direction if needed}
# !!trow{!!emph{Invocation}}{perl rotate file1 file2 @list1 @list2}
# !!table
#
# where:
#
# !!table
# !!arg{file1, file2}{are individual files to be rotated.}
# !!arg{list1, list2}{are ascii lists of files (flagged by @).}
# !!table
#
# !!head2 Action
#
# !!emph{rotate} rotates a series of frames by 90 degrees, and is often
# needed as a first step to get dispersion to run in Y as expected by
# pamela. It uses irot90 to rotate the frames by 90 degrees into a
# temporary file 'junk.sdf' then moves this back over the original file.
# This is to avoid corrupting the file.
#
# Because it uses a temporary file, it should be safe to kill it if you
# want, but remember that you will then end up with a mix of rotated and
# unrotated frames.
#
# !!end

use strict;

# Directories where various commands required are to be found
# Check existence of commands

my $STAR = $ENV{'STARLINK_DIR'};

my $FIG_DIR  = $STAR . '/bin/figaro';

-e "$FIG_DIR/irot90" || die "Failed to find $FIG_DIR/irot90.\n";

# Names of files that will be created by running this routine.

my $script = 'junkzzz';
my $log    = 'junkzzz.log';

# Interpret file name arguments

my $nfiles = 0;
my (@files,$arg);
while($arg = shift){
    if($arg =~ /^@(.*)/){
	my $list = $1;
	open(LIST, $list) || die "Can't open $list\n";
	while(<LIST>){
	    chop($files[$nfiles++] = $_);
	}
	close(FILES);
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
    s/.sdf$//;
    -e "${file}.sdf" || die "${file}.sdf does not exist!\n";
    -f "${file}.sdf" || die "${file}.sdf is not a plain file!\n";
    -r "${file}.sdf" || die "${file}.sfd is not readable!\n";
    -w "${file}.sdf" || die "${file}.sdf is not writeable!\n";
}

# Generate a script to rotate frames.

print "Rotating  ... script: $script --> log: $log.\n\n";

open(SCRIPT,">$script") || die "Failed to open $script\n";
foreach $file (@files){
    print SCRIPT "$FIG_DIR/irot90 $file junk\n";
    print SCRIPT "\\mv junk.sdf $file.sdf\n";
}
close(SCRIPT);

# Run it

system("chmod +x $script; ./$script > $log");

exit;

