##---------------------------------------------------------------------------##
##  File:
##      @(#) EntMan.pm 1.6 97/09/15 14:58:19 @(#)
##  Author:
##      Earl Hood			ehood@medusa.acs.uci.edu
##  Description:
##      This file defines the SGML::EntMan class.
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
##	The following is an example of how to use this class:
##
##	    use SGML::EntMan;
##
##	    $entman = new SGML::EntMan;
##	    # ...
##---------------------------------------------------------------------------##

package SGML::EntMan;

use vars qw(@ISA @EXPORT $VERSION @SGML_CATALOG_FILES $PATHSEP);

use Exporter ();
@ISA = qw( Exporter );

@EXPORT = ();
$VERSION = "0.01";

use OSUtil;
use SGML::SOCat;
use SGML::FSI;

BEGIN {
    ## Grab environment variables
    @SGML_CATALOG_FILES = split(/$PATHSEP/o, $ENV{SGML_CATALOG_FILES});

    ## Read default catalogs.  $EnvCat is the SOCat object that
    ## represents the default catalogs read.  This object is shared
    ## among all instantiated EntMan objects.

    my $file;
    $EnvCat = new SGML::SOCat;
    foreach $file (@SGML_CATALOG_FILES) {
	$EnvCat->read_file($file);
    }
}

##**********************************************************************##
##	PUBLIC METHODS
##**********************************************************************##

##----------------------------------------------------------------------
##	new() constructor.
##
sub new {
    my $this = { };
    my $class = shift;
    bless $this, $class;

    ## Initialize main catalog

    $this->{Catalog} = new SGML::SOCat;

    ## Initialize hash for delegate catalogs.  Eack key is a sysid
    ## that would be returned from the main catalog if delegation
    ## has been specified.  Each value is the SOCat object pointed
    ## to by sysid.  This is basicly a cache of delegate catalogs
    ## to avoid reparsing.

    $this->{Delegates} = {};

    $this;
}

##----------------------------------------------------------------------
##	read_catalog() reads the catalog designated by the filename
##	passed in.  A 1 is returned on success, and a 0 on failure.
##
sub read_catalog {
    my $this = shift;
    my $fname = shift;

    $this->{Catalog}->read_file($fname);
}

##----------------------------------------------------------------------
##	read_catalog_handle() reads the catalog designated by the
##	filehandle passed in.  A 1 is returned on success, and a 0 on
##	failure.  A reference to a filehandle should passed in to avoid
##	problems with package scoping.
##
sub read_catalog_handle {
    my $this  = shift;
    my($fh, $fname) = @_;

    $this->{Catalog}->read_handle($fname);
}

##----------------------------------------------------------------------
##	open_entity() returns a filehandle reference to the entity
##	specified entity name, pubid, and/or sysid.
##
##	Usage:
##	    $fh = $entman->open_entity($name, $pubid, $sysid);
##	
##	undef is returned if entity could not be resolved, and a
##	warning message is printed to stderr.
##
##	If $name contains a '%' character, it is treated a parameter
##	entity name.
##
sub open_entity {
    my $this = shift;
    my($name, $in_pubid, $in_sysid) = @_;

    $this->_open_entity($name, $in_pubid, $in_sysid, $this->get_ent($name));
}

##----------------------------------------------------------------------
##	open_doctype() returns a filehandle reference to the doctype
##	specified entity name, pubid, and/or sysid.
##
##	Usage:
##	    $fh = $entman->open_doctype($name, $pubid, $sysid);
##	
##	undef is returned if doctype could not be resolved, and a
##	warning message is printed to stderr.
##
sub open_doctype {
    my $this = shift;
    my($name, $in_pubid, $in_sysid) = @_;

    $this->_open_entity($name, $in_pubid, $in_sysid,
			$this->get_doctype($name));
}

##----------------------------------------------------------------------
##	open_public_id() returns a filehandle reference to the
##	entity denoted by a public id.
##
##	Usage:
##	    $fh = $entman->open_public_id($pubid);
##	
##	undef is returned if public id could not be resolved, and a
##	warning message is printed to stderr.
##
sub open_public_id {
    my $this = shift;
    $this->_open_entity('', shift);
}

##----------------------------------------------------------------------
##	open_system_id() returns a filehandle reference to the
##	entity denoted by a system id.
##
##	Usage:
##	    $fh = $entman->open_system_id($sysid);
##	
##	undef is returned if system id could not be resolved, and a
##	warning message is printed to stderr.
##
sub open_system_id {
    my $this = shift;
    $this->_open_entity('', '', shift);
}

##**********************************************************************##
##	semi-PUBLIC METHODS
##**********************************************************************##

##----------------------------------------------------------------------
##	get_public() resolves a public id to a (system id, base,
##	override) set.
##	
##	The following algorithm is used to resolve the public_id:
##
##	1. Check $this->{Catalog}->get_public(), else
##	2. Check $this->{Catalog}->get_delegate(), and if a sysid
##	   returned, use sysid as catalog to resolve pubid, else,
##	3. Check $EnvCat->get_public(), else,
##	4. Check $EnvCat->get_delegate.
##
sub get_public {
    my $this = shift;
    my $in_pubid = shift;
    my($sysid, $base, $o);

    BLK: {
	## Check for public entry in Catalog
	($sysid, $base, $o) = $this->{Catalog}->get_public($in_pubid);
	last BLK  if $sysid;

	## Check if delegating
	($sysid, $base) = $this->{Catalog}->get_delegate($in_pubid);
	if ($sysid) {
	    ($sysid, $base, $o) =
		$this->resolve_delegate($sysid, $base, $in_pubid);
		last BLK;
	}

	## Check for public entry in environment catalog(s)
	($sysid, $base, $o) = $EnvCat->get_public($in_pubid);
	last BLK  if $sysid;

	## Check if delegating from environment catalog(s)
	($sysid, $base) = $EnvCat->get_delegate($in_pubid);
	if ($sysid) {
	    ($sysid, $base, $o) =
		$this->resolve_delegate($sysid, $base, $in_pubid);
		last BLK;
	}

    }

    ($sysid, $base, $o);
}

##----------------------------------------------------------------------
##	get_system() returns a new system id for a system id if a
##	mapping is defined in catalog(s).  Null values are returned
##	in no mapping exists.
##
sub get_system {
    my $this = shift;
    my $in_sysid = shift;
    my($sysid, $base, $o);

    ($sysid, $base, $o) = $this->{Catalog}->get_system($in_sysid);
    ($sysid, $base, $o) = $EnvCat->get_system($in_sysid)
	unless $sysid;
    ($sysid, $base, $o);
}

##----------------------------------------------------------------------
##	get_ent resolves an entity name to a system id.
##
##	If $name contains a '%' character, it is treated a parameter
##	entity name.
##
sub get_ent {
    my $this = shift;
    my $name = shift;
    my($sysid, $base, $o);
    my $isparm = $name =~ s/%//;

    if ($name) {
	if ($isparm) {
	    ($sysid, $base, $o) = $this->{Catalog}->get_parm_ent($name);
	    ($sysid, $base, $o) = $EnvCat->get_parm_ent($name)
		unless $sysid;
	} else {
	    ($sysid, $base, $o) = $this->{Catalog}->get_gen_ent($name);
	    ($sysid, $base, $o) = $EnvCat->get_gen_ent($name)
		unless $sysid;
	}
    }
    ($sysid, $base, $o);
}

##----------------------------------------------------------------------
##	get_doctype resolves a doctype name to a system id.
##
sub get_doctype {
    my $this = shift;
    my $name = shift;
    my($sysid, $base, $o);

    if ($name) {
	($sysid, $base, $o) = $this->{Catalog}->get_doctype($name);
	($sysid, $base, $o) = $EnvCat->get_doctype($name)
	    unless $sysid;
    }
    ($sysid, $base, $o);
}

##----------------------------------------------------------------------
##	resolve_delegate() reolves a public id to a system id given
##	the system id, and base, of the catalog to read.  The
##	method returns (system id, base, override) for the public id.
##	The values will be null if unable to resolve.
##
sub resolve_delegate {
    my $this = shift;
    my($csysid, $cbase, $in_pubid) = @_;
    my($sysid, $base, $o);
    my($cat, $file);

    BLK: {
	## Read catalog if not cached

	if (not $cat = $this->{Delegates}{$csysid}) {
	    $file = &OpenSysId($csysid, $cbase);
	    if (!$file) {
		$this->_errMsg(qq{Error: Unable to open "$csysid"});
		last BLK;
	    }
	    $cat = $this->{Delegates}{$csysid} = new SGML::SOCat;
	    $cat->read_handle($file);
	    $file->close;
	}

	## Check if there is a public entry.

	($sysid, $base, $o) = $cat->get_public($in_pubid);
	last BLK  if $sysid;

	## Check if delegating (again)

	($csysid, $cbase) = $cat->get_delegate($in_pubid);
	last BLK  unless $csysid;

	($sysid, $base, $o) =
	    $this->resolve_delegate($csysid, $cbase, $in_pubid);
    }

    ($sysid, $base, $o);
}

##**********************************************************************##
##	PRIVATE METHODS
##**********************************************************************##

##----------------------------------------------------------------------
##
sub _open_entity {
    my $this = shift;
    my($name, $in_pubid, $in_sysid, $esysid, $ebase, $eo) = @_;

    $name = ''      unless $name =~ /\S/;
    $in_pubid = ''  unless $in_pubid =~ /\S/;
    $in_sysid = ''  unless $in_sysid =~ /\S/;

    ## Check if arguments valid
    unless ($name or $in_pubid or $in_sysid) {
	$this->_errMsg("Error: Null arguments passed to _open_entity");
	return undef;
    }

    my($psysid, $pbase, $po);
    my($ssysid, $sbase);
    my($esysid, $ebase, $eo);
    my $fh = undef;

    ## Look up name, pubid and sysid
    ($psysid, $pbase, $po) = $this->get_public($in_pubid)
	if $in_pubid;
    ($ssysid, $sbase) = $this->get_system($in_sysid)
	if $in_sysid;

    ## Open entity.
    BLK: {
	## Check if using pubid
	if ($in_pubid and (!$sysid or $po)) {
	    if (!defined($fh = &OpenSysId($psysid, $pbase))) {
		$this->_errMsg(qq{Unable to open "$in_pubid" => "$psysid"});
	    }
	    last BLK;
	}

	## Check if using entity name
	if ($name and (!$sysid or $eo)) {
	    if (!defined($fh = &OpenSysId($esysid, $ebase))) {
		$this->_errMsg(qq{Unable to open "$name" => "$esysid"});
	    }
	    last BLK;
	}

	## Check sysid
	if ($ssysid) {
	    if (!defined($fh = &OpenSysId($ssysid, $sbase))) {
		$this->_errMsg(qq{Unable to open "$in_sysid" => "$ssysid"});
	    }
	} else {
	    if (!defined($fh = &OpenSysId($in_sysid))) {
		$this->_errMsg(qq{Unable to open "$in_sysid"});
	    }
	}
	last BLK;
    }
    $fh;
}

##----------------------------------------------------------------------
sub _errMsg {
    my $this = shift;
    warn ref($this), ":", @_, "\n";
}

##----------------------------------------------------------------------
1;

