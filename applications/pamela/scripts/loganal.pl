#!/usr/bin/perl
#
# !!begin
# !!title  Reduction summary script
# !!author T.R.Marsh
# !!created 14 January 2001
# !!revised 12 December 2005
# !!root   loganal
# !!index  loganal.pl
# !!descr  Perl scipt to print out main results of running reduce
# !!head1  Reduction summary script
#
# !!table
# !!trow{!!emph{Name:}}{loganal}
# !!trow{!!emph{Purpose:}}{to print out most important parts of reduction log.}
# !!trow{!!emph{Invocation}}{perl loganal <reduction log>}
# !!table
#
# !!emph{loganal} is useful for examining the results of running reduce
# which get saved to a file. This file is usually large and tedious
# to look through. !!emph{loganal} collects and prints out the most useful
# indicators of problems for each routine such as the mean RMS for
# skyfit, and the positions found by track and skymov.
#
# !!emph{loganal} takes one argument, which is the name of the logfile.
#
# !!end

use strict;
use warnings;

(@ARGV == 1) or die "usage: loganal <reduction log>\n";

my $log = shift;

my ($command, $nx, %track, $file, $chi, $rms, $nrej, %xpos, %skymov);
my ($xpos, $shift, $elim, $rej, $ratio, $nratio, $maxblock, %skyfit);
my (%profit, %optext, %extopt, $ntrack, $nskymov, $nskyfit, $nprofit);
my ($noptext, %readout);
$command = "";

open(LOG,$log) or die "Could not open $log\n";
while(<LOG>){

    if(/^Starting file =\s*(\S+)/){

	$file = $1;

    }elsif(/^Readout noise =\s*(\S+)/){
	
	$readout{$file} = $1;
	
    }elsif(/^(track|skymov|skyfit|profit|optext|extopt)/){
	
	$command = $1;

    }elsif($command eq "track" && /Chi-squared of fit =\s*(\S+)/){
	
	$track{$file}->{CHI} = $1;

    }elsif($command eq "track" && /RMS deviation =\s*(\S+) pixels/){

	$track{$file}->{RMS} = $1;

    }elsif($command eq "track" && /Y = [\d\.]+, X = ([-+\d\.e]+)/){

	push @{$track{$file}->{XPOS}}, $1;

    }elsif($command eq "track" && /^Rejected (\d+)/){

	$track{$file}->{NREJ} = $1;

    }elsif($command eq "skymov" && /^Object position =\s*(\S+), shift =\s*(\S+)/){

	$skymov{$file}->{XPOS}  = $1;

	$skymov{$file}->{SHIFT} = $2;

    }elsif($command eq "skymov" && /^(\S+) sky pixels/){

	$skymov{$file}->{ELIM}  = $1;

    }elsif($command eq "skyfit" && /Average RMS =\s*(\S+)\s*,\s*(\S+)/){

	$skyfit{$file}->{RMS}  = $1;
	$skyfit{$file}->{REJ}  = $2;

    }elsif($command eq "profit" && /Reduced Chi-squared of fit =\s*(\S+)/){

	$profit{$file}->{CHI}  = $1;

    }elsif($command eq "profit" && /rejected points =\s*(\S+)/){

	$profit{$file}->{REJ}  = $1;

    }elsif($command eq "optext" && /pixels rejected =\s*(\S+)/){

	$optext{$file}->{REJ}  = $1;

    }elsif($command eq "extopt" && /^Poly-terms.*\) =\s*(\S+)/){

	$extopt{$file}->{RATIO}   += $1;
	$extopt{$file}->{NRATIO}++;

    }elsif($command eq "extopt" && /^Number of cycles.*:\s*(\S+)/){

	if(defined $extopt{$file}->{MAXBLOCK}){
	    $extopt{$file}->{MAXBLOCK} = $1 > $extopt{$file}->{MAXBLOCK} ? $1 : $extopt{$file}->{MAXBLOCK};
	}else{
	    $extopt{$file}->{MAXBLOCK} = $1;
	}

    }elsif($command eq "extopt" && /^Total pixels rejected:\s*(\S+)/){

	$extopt{$file}->{NREJ} = $1;

    }
}
close(LOG);

if(%readout){
    print "\nreadout noise values:\n\n";
    foreach $file (sort keys %readout){
	printf "  READOUT: file=%8s, readout noise = %5.2f\n",
	$file,$readout{$file};
    }
}

if(%track){
    print "\n\ntrack results:\n\n";
    foreach $file (sort keys %track){
	my $ref = $track{$file};
	if(defined $ref->{RMS} && defined $ref->{NREJ} && $ref->{CHI}){
	    if(defined $ref->{XPOS} && @{$ref->{XPOS}} == 3){
		printf "  TRACK: file=%8s, chi**2=%4.4f, RMS=%5.5f, nrej=%3d, xpos=%7.2f, %7.2f, %7.2f\n",
		$file,$ref->{CHI},$ref->{RMS},$ref->{NREJ},$ref->{XPOS}[0],$ref->{XPOS}[1],$ref->{XPOS}[2];
	    }else{
		printf "  TRACK: file=%8s, chi**2=%4.4f, RMS=%5.5f, xposition failure\n",
		$file,$ref->{CHI},$ref->{RMS},$ref->{NREJ};
	    }
	}else{
	    print "  Failed to find all track parameters for file = $file\n";
	}

    }
}

if(%skymov){
    print "\n\nskymov results:\n\n";
    foreach $file (sort keys %skymov){
	my $ref = $skymov{$file};
	if(defined $ref->{XPOS} && defined $ref->{SHIFT} && defined $ref->{ELIM}){
	    printf "  SKYMOV: file=%8s, position = %7.2f, shift=%4.4f, eliminated=%3d\n",
	    $file,$ref->{XPOS},$ref->{SHIFT},$ref->{ELIM};
	}else{
	    print "  Failed to find all skymov parameters for file = $file\n";
	}
    }
}

if(%skyfit){
    print "\n\nskyfit results:\n\n";
    foreach $file (sort keys %skyfit){
	my $ref = $skyfit{$file};
	if(defined $ref->{RMS} && defined $ref->{REJ}){
	    printf "  SKYFIT: file=%8s, mean RMS=%3.3f, number rejected=%5d\n", 
	    $file,$ref->{RMS},$ref->{REJ};
	}else{
	    print "  Failed to find all skyfit parameters for file = $file\n";
	}
    }
}

if(%profit){
    print "\n\nprofit results:\n\n";
    foreach $file (sort keys %profit){
	my $ref = $profit{$file};
	if(defined $ref->{CHI} && defined $ref->{REJ}){
	    printf "  PROFIT: file=%8s, final Chi**2=%4.4f, number rejected=%5d\n", 
	    $file,$ref->{CHI},$ref->{REJ};
	}else{
	    print "  Failed to find all profit parameters for file = $file\n";
	}
    }
}

if(%optext){
    print "\n\noptext results:\n\n";
    foreach $file (sort keys %optext){
	my $ref = $optext{$file};
	if(defined $ref->{REJ}){
	    printf "  OPTEXT: file=%8s, number rejected=%5d\n",
	    $file,$ref->{REJ};
	}else{
	    print "  Failed to find all optext parameters for file = $file\n";
	}
    }
}

if(%extopt){
    print "\n\nextopt results:\n\n";
    foreach $file (sort keys %extopt){
	my $ref = $extopt{$file};
	if(defined $ref->{NREJ} && defined $ref->{RATIO} && defined $ref->{NRATIO} && defined $ref->{MAXBLOCK}){
	    printf "  EXTOPT: file=%8s, rejected=%5d, mean ratio=%5.2f, max block rej=%4d\n",
	    $file,$ref->{NREJ},$ref->{RATIO}/$ref->{NRATIO},$ref->{MAXBLOCK};
	}else{
	    print "  Failed to find all optext parameters for file = $file\n";
	}
    }
}

exit;
