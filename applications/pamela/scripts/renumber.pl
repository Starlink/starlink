#!/usr/bin/perl
#
# !!begin
# !!title  Renumbering script
# !!author T.R. Marsh
# !!created  14 January 2001
# !!revised  08 December 2005
# !!root   renumber
# !!index  renumber.pl
# !!descr  Perl script for renumbering files according to record number
# !!head1  Script for renumbering files by record number
#
# It is sometimes helpful to have files named by their 'run number' if
# it exists. This can make it easier to correlate with observing logs
# for instance. This script is meant to help with that, assuming that
# you have previously run !!ref{fixhead.html}{fixhead} on the files.
# and that run numbers have been located. It does nothing if it cannot find
# the run number
#
# !!head2 Arguments
# !!table
## !!arg{file1, file2}{are individual files.}
# !!arg{list1, list2}{are ascii lists of files (flagged by @).}
# !!table
#
# !!head2 Action
#
# !!emph{renumber} searches out record numbers of files and then changes
# the name to r####.sdf where #### is the number. It will not rename
# to any file which already exists.
#
# The script will generate files called 'junkzzz' and 'junkzzz.log'
# !!end

use warnings;
use strict;

# Directories where various commands required are to be found

my $STAR_DIR = $ENV{'STARLINK_DIR'} . '/bin';

# Check existence of commands

-e "$STAR_DIR/hdstrace" or die "Failed to find $STAR_DIR/hdstrace.\n";

# Names of files that will be created by running this routine.

my $script = 'junkzzz';
my $log    = 'junkzzz.log';

# Interpret file name arguments

my $nfiles = 0;
my ($arg, @files);
while($arg = shift){
    if($arg =~ /^@(.*)/){
	my $list = $1;
	open(LIST, $list) || die "Can't open $list\n";
	while(<LIST>){
	    chop($files[$nfiles++] = $_);
	}
	close(LIST);
    }else{
	$files[$nfiles++] = $arg;
    }
}

$nfiles || die "No file names loaded.\n";

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
    -e "${file}.sdf" || die "${file}.sdf does not exist!\n";
    -f "${file}.sdf" || die "${file}.sdf is not a plain file!\n";
    -r "${file}.sdf" || die "${file}.sfd is not readable!\n";
}

# Generate a script to list header info. This part would have
# to be altered if the header was not in .MORE.FITS. Although only
# one header item is being looked for, printing out the whole headers
# means that we do not have to know the position of the item in the
# headers.

print "Listing headers to $log  ...\n\n";

open(SCRIPT,">$script") || die "Failed to open $script\n";
foreach $file (@files){
    print SCRIPT "echo \"File name = $file\"\n";
    print SCRIPT "$STAR_DIR/hdstrace $file.more.pamela\n";
}
close(SCRIPT);

# Run it
system("chmod +x $script; ./$script > $log");

# We are looking for an item of the form:
#
#  NUMRUN  <_INTEGER>  686879

open(LOG, $log) || die "Can't open $log\n";
while(<LOG>){
    if(/^File name = (\S*)/){
	$file = $1;
    }
    if(/^\s*NUMRUN\s*<_INTEGER>\s*(\d*)/){
	if(-e "r$1.sdf"){
	    print "Output r$1.sdf already exists and will not be overwritten\n";
	}else{
	    rename "$file.sdf", "r$1.sdf";
	}
    }
}
close(LOG);

print "Done.\n\n";

exit;




