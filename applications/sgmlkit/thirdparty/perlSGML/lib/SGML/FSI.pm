##---------------------------------------------------------------------------##
##  File:
##      @(#) FSI.pm 1.6 97/09/18 14:36:01 @(#)
##  Author:
##      Earl Hood			ehood@medusa.acs.uci.edu
##  Description:
##      This file defines the SGML::FSI module.
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
##  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
##---------------------------------------------------------------------------##
##  Usage:
##	The following is an example of how to use this module:
##
##	    use FileHandle;
##	    use SGML::FSI qw( &OpenSysId );
##
##	    $fh = OpenSysId($sysid, $base);
##	    # ...
##
##  Notes:
##	Currently, this module does not really support FSIs.  The
##	module exists to provide a stable(?) interface to resolving
##	sysids to perl filehandles.  Someday, FSI support will be
##	added as time permits and decent documentation exists.
##
##	The module's primary user will be the entity manager.
##
##	All sysids are treated as pathnames of files.
##---------------------------------------------------------------------------##

package SGML::FSI;

use vars qw(@ISA @EXPORT @EXPORT_OK $VERSION @SGML_SEARCH_PATH $PATHSEP);

use Exporter ();
@ISA = qw( Exporter );

@EXPORT = qw(
    &OpenSysId
);
@EXPORT_OK = qw(
    &ResolveSysId
);
$VERSION = "0.04";

use FileHandle;
use OSUtil;

## Grab environment variables

# SGML_SEARCH_PATH defines a list of paths for searching
# for relative file sysids.

@SGML_SEARCH_PATH = (split(/$PATHSEP/o, $ENV{P_SGML_PATH}),
		     split(/$PATHSEP/o, $ENV{SGML_SEARCH_PATH}));

##**********************************************************************##
##	PUBLIC ROUTINES
##**********************************************************************##

##----------------------------------------------------------------------
##	ResolveSysId() returns a resolved system identifier based upon
##	passed in sysid and its base.  The behavior of this function
##	is a follows:
##
##	    o	If $sysid is absolute, it is the return value.
##	    o	If $base is defined, the return value is $base
##		applied to $sysid to for a new sysid.
##	    o	Else, SGML_SEARCH_PATH envariable is searched
##		for $sysid.  If $sysid exists in SGML_SEARCH_PATH,
##		it returns the resolved pathname.  Else, $sysid
##		is returned.
##
##	This function appears to be potentially a big noop, but
##	is useful for have a base indentifier applied to a relative
##	sysid and to have it do the SGML_SEARCH_PATH search if
##	required.
##
sub ResolveSysId {
    my($sysid, $base) = @_;

    return undef  unless $sysid =~ /\S/;

    ## Check if sysid an absolute pathname
    if (OSUtil::is_absolute_path($sysid)) {
	return $sysid;
    }

    ## See if base can be used
    if ($base) {
	return $base . $DIRSEP . $sysid;
    }

    ## See if sysid in current directory
    if (-e $sysid) {
	return $sysid;
    }

    ## If reached here, got to search for sysid
    my $pathname;
    foreach (@SGML_SEARCH_PATH) {
	$pathname = $_ . $DIRSEP . $sysid;
	if (-e $pathname) {
	    return $pathname;
	}
    }

    undef;
}


##----------------------------------------------------------------------
##	OpenSysId() returns a reference to a Perl filehandle for a
##	sysid.  undef is returned if sysid could not be opened.
##
sub OpenSysId {
    my($sysid, $base) = @_;

    return undef  unless $sysid =~ /\S/;
    return new FileHandle ResolveSysId($sysid, $base);
}

##----------------------------------------------------------------------
1;

