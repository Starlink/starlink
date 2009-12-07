#!/usr/bin/perl -w
#
# !!begin 
# !!title  Groups data by source type
# !!author T.R.Marsh
# !!created  10 December 2005
# !!root   splitbysource
# !!index  splitbysource.pl 
# !!descr  Perl script for grouping data by source
# !!head1 Data grouping script
#
# Assuming that you have run !!ref{fixhead.html}{fixhead}, you might have
# the item .more.pamela.source in each file which should be set to one
# of ARC, FLAT, DARK, BIAS, DATA, SKY. This script will then move the
# files into directories names 'flats', 'biases', 'darks', 'skies'
# and 'data' (both ARC and DATA go into this last one). In each case
# the data will be moved into subdirectories called 'night1', 'night2'
# etc according to the value of .more.pamela.night
#
# Nothing is done if the source or night number are undefined or if the
# directory does not exist.
#
# !!head2 Invocation
#
# splitbysource.pl file1 file2 ...
#
# !!head2 Created files
#
# This will make files called zzz_splitbysource and zzz_splitbysource.log
#
# !!end

use strict; 

@ARGV or die "usage: file1 file2 ...\n";

# Directories where various commands required are to be found
# Check existence of commands

# Top level of STARLINK

my $STAR     = $ENV{'STARLINK_DIR'};

# Check for existence of commands that will be used

-e "$STAR/bin/hdstrace"      or die "Failed to find $STAR/bin/hdstrace.\n";

# Names of files that will be created by running this routine.

my $script = 'zzz_splitbysource';
my $log    = 'zzz_splitbysource.log';

# Generate a script to list header info. This part would have
# to be altered if the header was not in .MORE.FITS. By printing out
# the whole of the FITS header, the routine should be less sensitive 
# to the exact posistion of items, but still may need varying according
# to the data format.

print "Listing headers to $log  ...\n\n";

open(SCRIPT,">$script") or die "Failed to open $script\n";
my ($file,@files);
foreach $file (@ARGV){
    $file =~ s/\.sdf//;
    push @files, $file;
    if(-e "$file.sdf"){
	print SCRIPT "echo File name = $file\n";
	print SCRIPT "$STAR/bin/hdstrace $file.more.pamela\n"; 
    }else{
	die "No file = $file.sdf exists!\n";
    }
}
close(SCRIPT);

# Run it

system("chmod +x $script; ./$script > $log");

open(LOG, $log) || die "Can't open $log\n";
my (%source, %night);
while(<LOG>){
    if(/^File name = (\S*)/){
	$file  = $1;
    }elsif(/NIGHT\s*<_INTEGER>\s*(\d+)/){
	$night{$file} = $1;
    }elsif(/SOURCE\s*<_CHAR\*4>\s*\'(\S*)\'/){
	$source{$file} = $1;
    }
}
close(LOG);

# Now move those files with the correct information

print "Fixing up headers ...\n\n";

open(SCRIPT,">$script") || die "Failed to open $script\n";

# Create items (if they are already present then this generates
# non-fatal errors when run).

my ($dir1, $dir2);
foreach $file (@files){

    if(defined $source{$file} && defined $night{$file}){
	
	if($source{$file} =~ /DATA/ || $source{$file} =~ /ARC/){
	    $dir1 = "data";
	}elsif($source{$file} =~ /BIAS/){
	    $dir1 = "biases";
	}elsif($source{$file} =~ /FLAT/){
	    $dir1 = "flats";
	}elsif($source{$file} =~ /DARK/){
	    $dir1 = "darks";
	}elsif($source{$file} =~ /SKY/){
	    $dir1 = "skies";
	}else{
	    print "Could not recognise source = ",$source{$file}," in $file.sdf\n";
	    print "Nothing will be done\n";
	    next;
	}
	$dir2 = "night$night{$file}";

	(-e $dir1) or mkdir $dir1;
	(-e "$dir1/$dir2") or mkdir "$dir1/$dir2";
	
	rename "$file.sdf", "$dir1/$dir2/$file.sdf";
	print "$file.sdf ---> $dir1/$dir2/$file.sdf\n";

    }else{

	print "One or both of 'source' and 'night' were not defined in $file.sdf\n";
	print "Nothing will be done\n";

    }
}

exit;




