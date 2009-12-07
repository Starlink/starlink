#!/usr/bin/perl
#
# !!begin 
# !!title  Repeater script
# !!author T.R. Marsh
# !!created 14 January 2001
# !!revised 08 December 2005
# !!root   repet 
# !!index  repet.pl 
# !!descr  Perl script for repeting a command on a set of files 
# !!head1  Repeating script
#
# On many occasions one has to subtract a constant from a series of
# files, or perhaps divide a set of files by another file. !!emph{repet}
# is intended to facilitate such operations, although if you are a
# C-shell whizz you can do the same thing with 'foreach'.
#
# For instance, suppose you want to subtract the same file 'sfile' from
# a series of files contained in a list 'flist', then the command:
#
# repet "$STARLINK_DIR/bin/figaro/isub %%% sfile %%%" @flist
#
# will do it. Note that although this is an NDF/Figaro command, there
# is nothing especially NDF-based about !!emph{repet}, and, for instance
# it does not attempt to remove .sdf extensions or check for the existence
# of files. Thus:
#
# repet "mv %%%.sdf\;1 %%%.sdf" @flist
# 
# can be used to change the names of the form r345.sdf;1 to r345.sdf
# thus removing the irritating ; which needs to be escaped all the time.
# If something goes wrong, check the junkzzz script that is produced.
#
# Why "repet" and not "repeat"? because there is a built-in shell
# command of that name.
#
# !!head2 Arguments
#
# !!table
# !!arg{command}{the command to be carried out.}
# !!arg{file1, file2}{are individual files.}
# !!arg{list1, list2}{are ascii lists of files (flagged by @).}
# !!table
#
# !!end

use strict;

my $command = shift or die "No command supplied.\n";

# Interpret file name arguments

my $nfiles = 0;
my ($arg, @files):
while($arg = shift){
    if($arg =~ /^@(.*)/){
	$list = $1;
	open(LIST, $list) or
	    die "Can't open $list\n";
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

# OK down to business

print "Generating script junkzzz ...\n\n";

open(SCRIPT, ">junkzzz") || die "Can't open junkzzz\n";

print SCRIPT "#!/bin/csh -f\n\n";

my $temp;
foreach $file (@files){
    if($file =~ /\S/){
	$temp = $command;
	$temp =~ s/%%%/$file/g;
	print SCRIPT "$temp\n";
    }
}

print SCRIPT "exit\n\n";
close(SCRIPT);

print "Running script ...\n\n";

system("chmod +x junkzzz; ./junkzzz");

print "Finished.\n\n";

exit;





