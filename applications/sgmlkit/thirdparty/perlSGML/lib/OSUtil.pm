##---------------------------------------------------------------------------##
##  File:
##	@(#) OSUtil.pm 1.3 97/09/17 14:31:40 @(#)
##  Author:
##      Earl Hood       ehood@medusa.acs.uci.edu
##  Description:
##	A module for setting varaibles and defining routines
##	to help in OS specific type tasks.
##
##  Note:
##	Mac and VMS are probably not supported correctly.  Any
##	contributions welcome.
##---------------------------------------------------------------------------##
##    Copyright (C) 1996,1997	Earl Hood, ehood@medusa.acs.uci.edu
##
##    This program is free software; you can redistribute it and/or modify
##    it under the terms of the GNU General Public License as published by
##    the Free Software Foundation; either version 2 of the License, or
##    (at your option) any later version.
##
##    This program is distributed in the hope that it will be useful,
##    but WITHOUT ANY WARRANTY; without even the implied warranty of
##    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
##    GNU General Public License for more details.
##
##    You should have received a copy of the GNU General Public License
##    along with this program; if not, write to the Free Software
##    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
##---------------------------------------------------------------------------##

package OSUtil;

use vars qw($VERSION @ISA @EXPORT @EXPORT_OK);

use Exporter ();
@ISA = qw( Exporter );

$VERSION = "0.02";

@EXPORT = qw( &CLinit
	      $MSDOS $MACOS $UNIX $VMS $WINDOWS
	      $DIRSEP $DIRSEPRX $CURDIR
	      $PROG $PATHSEP
	      $OSType
	    );

@EXPORT_OK = qw( &path_join &path_split &is_absolute_path );

##---------------------------------------------------------------------------##
##	BEGIN block.
##
##	Variables set:
##
##	    $MSDOS	=> Set to 1 if running under MS-DOS/Windows
##	    $MACOS	=> Set to 1 if running under Mac
##	    $UNIX	=> Set to 1 if running under Unix
##	    $VMS 	=> Set to 1 if running under VMS
##	    $WINDOWS	=> Set to 1 if running under Windows
##	    $DIRSEP	=> Directory separator character
##	    $DIRSEPRX	=> Directory separator character for use in
##			   regular expressions.
##	    $CURDIR	=> Value representing current directory
##	    $PROG	=> Program name with leading pathname component
##			   stripped off.
##	    $PATHSEP	=> Recommend separator for a list of paths.
##
##	Notes:
##	    Do not know what to do about VMS.  Currently treat it
##	    like Unix.  Mac stuff may be incorrect.
##
BEGIN {
    %DirSep = (
	macos	=> ':',
	msdos	=> '\\',
	unix	=> '/',
	vms	=> '/',	# ??
	windows	=> '\\',
    );
    %CurDir = (
	macos	=> ':',	# ??
	msdos	=> '.',
	unix	=> '.',
	vms	=> '.',	# ??
	windows	=> '.',
    );
    %PathSep = (
	macos	=> ';',	# ??
	msdos	=> ';',
	unix	=> ':',
	vms	=> ':',	# ??
	windows	=> ';',
    );

    my $dontknow = 0;

    ## Init variables
    $MACOS	= 0;	$MSDOS	= 0;
    $UNIX	= 0;	$VMS	= 0;
    $WINDOWS	= 0;
    $DIRSEP	= '/';	$CURDIR = '.';
    $PATHSEP	= ':';

    ## See if ostype can be determined from osname in Config
    if (defined $^O) {
	$_ = $^O;
    } else {
	require Config;
	$_ = $Config::Config{'osname'};
    }

    if (/mac/i) {
	$MACOS = 1;
	$OSType = 'macos';
    } elsif (/vms/i) {
	$VMS = 1;
	$OSType = 'vms';
    } elsif (/msdos/i) {
	$MSDOS = 1;
	$OSType = 'msdos';
    } elsif (/mswin/i) {
	$WINDOWS = 1;  $MSDOS = 1;
	$OSType = 'windows';
    } elsif (/unix/i or
	     /aix/i or
	     /dynix/i or
	     /hpux/i or
	     /solaris/i or
	     /sunos/i or
	     /ultrix/i or
	     /linux/i) {
	$UNIX = 1;
	$OSType = 'unix';
    } else {
	$dontknow = 1;
    }

    ## If we do not know now what the ostype is, make a guess.
    if ($dontknow) {
	my($tmp);

	## MSDOG/Windoze
	if (($tmp = $ENV{'windir'}) and ($tmp =~ /[A-Z]:\\/i) and (-d $tmp)) {
	    $MSDOS = 1;
	    $WINDOWS = 1;
	    $OSType = 'windows';

	} elsif (($tmp = $ENV{'COMSPEC'}) and ($tmp =~ /[a-zA-Z]:\\/) and
		 (-e $tmp)) {
	    $MSDOS = 1;
	    if ($tmp =~ /win/i) {
		$WINDOWS = 1;
		$OSType = 'windows';
	    } else {
		$OSType = 'msdos';
	    }

	## MacOS
	} elsif (defined($MacPerl::Version)) {
	    $MACOS = 1;
	    $OSType = 'macos';

	## Unix (fallback case)
	} else {
	    $UNIX = 1;
	    $OSType = 'unix';
	}
    }

    ## Set other variables
    $DIRSEP = $DirSep{$OSType};
    if ($MSDOS or $WINDOWS) {
	$DIRSEPRX = "\\\/";
    } else {
	($DIRSEPRX = $DIRSEP) =~ s/(\W)/\\$1/g;
    }
    $CURDIR = $CurDir{$OSType};
    $PATHSEP = $PathSep{$OSType};

    ##	Store name of program
    ($PROG = $0) =~ s%.*[$DIRSEPRX]%%o;

    ##	Flag to prompt for command-line options on a Mac
    $MacCLPrompt = 1;
}

##---------------------------------------------------------------------------##
##	CLinit() initializes @ARGV.  Currently, it does nothing under
##	MSDOS and Unix.
##
##	If running under a Mac and the script is a droplet, command-line
##	options will be prompted for if $MacCLPrompt is set to a
##	non-zero value.
##
sub CLinit {

    ##	Ask for command-line options if script is a Mac droplet
    ##		Code taken from the MacPerl FAQ
    ##
    if ($MacCLPrompt && ( $MacPerl::Version =~ /Application$/ )) {

	# we're running from the app
	my( $cmdLine, @args );
	$cmdLine = &MacPerl::Ask( "Enter command line options:" );
	require "shellwords.pl";
	@args = &shellwords( $cmdLine );
	unshift( @::ARGV, @args );
    }
}

##---------------------------------------------------------------------------##
##	path_join takes an array of path components and returns a string
##	with components joined together by the directoy separator.
##
sub path_join {
    join($DIRSEP, @_);
}

##---------------------------------------------------------------------------##
##	path_split takes a string representing a pathname and splits
##	it into an array of components.  The pathname is interpreted
##	with respect to the OS we are running under.
##
sub path_split {
    split(/[$DIRSEPRX]/o, $_[0]);
}

##---------------------------------------------------------------------------##
##	is_absolute_path() returns true if a string is an absolute path
##
sub is_absolute_path {

    if ($MSDOS or $WINDOWS) {
	return $_[0] =~ /^(?:[a-z]:)?[\\\/]/i;
    }
    if ($MACOS) {		## Not sure about Mac
	return $_[0] =~ /^:/o;
    }
    if ($VMS) {			## Not sure about VMS
	return $_[0] =~ /^\w+:/i;
    }
    $_[0] =~ m|^/|o;    	## Unix
}

##---------------------------------------------------------------------------##
1;
