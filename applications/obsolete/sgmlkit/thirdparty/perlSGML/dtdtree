#! /usr/local/bin/perl
##---------------------------------------------------------------------------##
##  File:
##      @(#)  dtdtree 1.2 96/10/06 @(#)
##  Author:
##      Earl Hood       ehood@medusa.acs.uci.edu
##  Description:
## 	Perl program to output content hierarchy trees of SGML elements.
##---------------------------------------------------------------------------##
##  Copyright (C) 1994-1996  Earl Hood, ehood@medusa.acs.uci.edu
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
##  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
##---------------------------------------------------------------------------##


package main;

## Store name of program ##
($PROG = $0) =~ s/.*\///;

$VERSION = "1.3.1";

unshift(@INC, 'lib');
require "dtd.pl" || die "Unable to require dtd.pl\n";
require "newgetopt.pl" || die "Unable to require newgetopt.pl\n";

##-------------##
## Get options ##
##-------------##
&Usage() unless
    &NGetOpt(
	"catalog=s",
	"dtd=s",
	"treefile=s",
	"level=i",
	"verbose",
	"help"
    );
&Usage() if defined($opt_help);

$DTDFILE  = ($opt_dtd ? $opt_dtd : "");
$TREEFILE = ($opt_treefile ? $opt_treefile : "");
$MAPFILE  = $opt_catalog || "catalog";
$LEVEL    = $opt_level || 15;
$VERBOSE  = (defined($opt_verbose) ? 1 : 0);

if ($DTDFILE) {
    open(DTD_FILE, "< $DTDFILE") || die "Unable to open $DTDFILE\n";
    $DTD = "main'DTD_FILE";
} else {
    $DTD = 'STDIN';
    $DTDFILE = 'DTD';
}
if ($TREEFILE) {
    open(TREE_FILE, "> $TREEFILE") || die "Unable to create $TREEFILE\n";
    $TREE = 'TREE_FILE';
} else {
    $TREE = 'STDOUT';
}
&DTDset_verbosity() if $VERBOSE;

##----------##
## Read DTD ##
##----------##
    print STDERR "Reading $MAPFILE ...\n" if $VERBOSE;
&DTDread_catalog_files($MAPFILE);
    print STDERR "Finished $MAPFILE ...\n" if $VERBOSE;
    print STDERR "Reading $DTDFILE ...\n" if $VERBOSE;
&DTDread_dtd($DTD);
    print STDERR "Finished $DTDFILE ...\n" if $VERBOSE;

##-------------##
## Print Trees ##
##-------------##
if ($#ARGV >= 0) { @array = @ARGV; }
else { @array = &DTDget_top_elements(); }

select($TREE); $^ = Empty; $~ = ElementHead; $= = 10000000;
foreach $elem (@array) {
    print STDERR "Printing content tree for $elem ...\n" if $VERBOSE;
    $elem =~ tr/a-z/A-Z/;
    write;
    &DTDprint_tree($elem, $LEVEL, $TREE);
}

exit(0);

##---------##
## Formats ##
##---------##
format Empty=
.

format ElementHead=
------------------------------------------------------------------------------
@|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
$elem
------------------------------------------------------------------------------
.

##---------------------------##
## Usage description routine ##
##---------------------------##
sub Usage {
    print STDOUT <<EndOfUsage;
Usage: $PROG [options] <element> ...
Options:
  -catalog <filename>	: Use <filename> as entity map file.  Defaults to
		          "catalog".
  -dtd <filename>	: Use <filename> as the SGML dtd to parse.  Otherwise
		          read from STDIN.
  -help			: Print this usage message.
  -level <#>		: Only go down to tree level <#>.  Default tree level
		          depth is 15.
  -treefile <filename>	: Output element content tree(s) to <filename>.
		          Otherwise print to STDOUT.
  -verbose		: Print to STDERR what is going on.

Description
  dtdtree outputs the content hierarchy trees of SGML elements defined in a
  DTD.  Any strings that are not part of the command-line options are the
  elements to output trees for.  If no elements are specified, then the
  tree(s) for the top-most element(s) are used.

Version: $VERSION
dtd.pl Version: $dtd'VERSION

  Copyright (C) 1994-1996  Earl Hood, ehood\@medusa.acs.uci.edu
  dtdtree comes with ABSOLUTELY NO WARRANTY and dtdtree may be copied only
  under the terms of the GNU General Public License (version 2, or later),
  which may be found in the distribution.

EndOfUsage
    exit(0);
}
