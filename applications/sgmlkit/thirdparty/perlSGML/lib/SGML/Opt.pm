##---------------------------------------------------------------------------##
##  File:
##      @(#)  Opt.pm 1.4 97/09/15 @(#)
##  Author:
##      Earl Hood			ehood@medusa.acs.uci.edu
##  Description:
##      This file defines the SGML::Opt class.
##---------------------------------------------------------------------------##
##  Copyright (C) 1996,1997	Earl Hood, ehood@medusa.acs.uci.edu
##
##  This program is free software; you can redistribute it and/or modify
##  it under the terms of the GNU General Public License as published by
##  the Free Software Foundation; either version 2 of the License, or
##  (at your option) any later version.
## 
##  This program is distributed in the hope that it will be useful,
##  but WITHOUT ANY WARRANTY; without even the implied warranty of
##  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
##  GNU General Public License for more details.
##  
##  You should have received a copy of the GNU General Public License
##  along with this program; if not, write to the Free Software
##  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
##  USA
##---------------------------------------------------------------------------##

package SGML::Opt;

##------------------------------------------------------------------------
##	The SGML::Opt module is for programs built using the
##	SGML Perl modules.  This package is designed to provide a
##	common interface to parsing the command-line.  The package
##	already includes common options for SGML::* based programs.
##	Each program can specify there own additional arguments that
##	should be parsed.
##
##	Usage:
##	    use SGML::Opt;
##
##	    AddOptions('opt1=s', "opt1 description",
##	               'opt2=s', "opt2 description");
##	    GetOptions();
##
##	    $opt1_string = $OptValues{'opt1'};
##	    # etc ...
##
##	The syntax of specifying command-line option type is the same
##	as document in the Getopt::Long module (this module inherits
##	from the Getopt::Long module).  All the command-line
##	option values will be stored in the %OptValues hash.  This
##	hash is automatically exported during the 'use' operation.
##	The hash is indexed by the name of the option.
##
##	The following options are predefined by this module:
##
##	    catalog		=> Catalog entity map files
##	    ignore		=> Parmater ents to set to "IGNORE"
##	    include		=> Parmater ents to set to "INCLUDE"
##
##	    debug|verbose	=> Debugging flag
##	    help		=> Help flag
##
##	The caller is responsible for acting upon any option defined
##	on the command-line.
##------------------------------------------------------------------------

use Exporter ();
use Getopt::Long;
@ISA = qw( Exporter GetOpt::Long );

@EXPORT = qw(
    &GetOptions
    &AddOptions
    &Usage
    %OptValues
    $Prog $ProgVersion
    $Debug $Help
    $Synopsis $Description $CopyYears
    @Catalogs
    @IncParmEnts
    @IgnParmEnts
);

$VERSION = "0.02";

use OSUtil;

##------------------------------------------------------------------------
BEGIN {
    ## Define default options
    %_options = (

	"catalog=s@"	=> "Entity mapping catalog",
	"ignore=s@"	=> qq(Set parameter entity to "IGNORE"),
	"include=s@"	=> qq(Set parameter entity to "INCLUDE"),

	"debug|verbose"	=> "Turn on debugging",
	"help"   	=> "Get help",

    );

    ## Init export variables
    %OptValues  = ();

    $Prog 	= $PROG;  # just copy from OSUtil

    $Debug	= 0;
    $Help	= 0;

    $Synopsis	= "$Prog [options]";
    $Description= "";
    $CopyYears	= "1997";

    ## Init private variables
    $_optspec_w	= 20;   	# Width for option spec for Usage
}

##------------------------------------------------------------------------
##	GetOptions takes 2 array references.  The first defines
##	any option specs, and the second a brief description of
##	the options.
##
sub GetOptions {
    AddOptions(@_);

    ## Explicitly call GetOpt's routine to do the actual parsing of @ARGV
    $retcode = Getopt::Long::GetOptions(\%OptValues, keys %_options);

    ## Set export variables
    @Catalogs	= @{$OptValues{"catalog"}};
    @IncParmEnts= @{$OptValues{"include"}};
    @IgnParmEnts= @{$OptValues{"ignore"}};

    $Debug	= $OptValues{"debug"}	     if $OptValues{"debug"};
    $Help	= $OptValues{"help"}	     if $OptValues{"help"};

    $retcode;
}

##------------------------------------------------------------------------
sub Usage {
    my($opt, $v, $o);
    my(@txt);
    my $fmt1 = "%${_optspec_w}s : %s\n";
    my $fmtn = "%${_optspec_w}s   %s\n";

    print STDOUT "Synopsis: $Synopsis\n";
    print STDOUT "Options:\n";
    foreach (sort keys %_options) {
	if (/([=:])(.)/) {
	    $opt = $`;  $o = $1;  $v = $2;

	    if      ($v eq 'i') {
		$v = '<int>';
	    } elsif ($v eq 's') {
		$v = '<str>';
	    } elsif ($v eq 'f') {
		$v = '<float>';
	    } else {
		$v = "<$v>";
	    }
	    if ($o eq ':') {
		$v = " [$v]";
	    } else {
		$v = " $v";
	    }

	} else {
	    $opt = $_;  $v = '';
	}
	@txt = split(/\n/, $_options{$_});
	print STDOUT sprintf($fmt1, "-$opt$v", shift(@txt));
	while (@txt) {
	    print STDOUT sprintf($fmtn, "", shift(@txt));
	}
    }
    if ($Description) {
	print STDOUT "Description:\n", $Description;
	print STDOUT <<"EndOfCopy";

  v$ProgVersion
  Copyright (C) $CopyYears  Earl Hood, ehood\@medusa.acs.uci.edu
  $Prog comes with ABSOLUTELY NO WARRANTY and $Prog may be
  copied only under the terms of the GNU General Public License
  (version 2, or later), which may be found in the distribution.
EndOfCopy

    }
}

##------------------------------------------------------------------------
##	AddOptions adds option specifications for command-line parsing.
##
sub AddOptions {
    my($spec, $desc);

    while (@_) {
	$spec = shift;
	$desc = shift;
	$_options{$spec} = $desc;
    }
}

##------------------------------------------------------------------------

1;

