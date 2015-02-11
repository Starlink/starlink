##---------------------------------------------------------------------------##
##  File:
##	@(#) ISO8859.pm 1.2 97/09/15 14:58:20 @(#)
##  Author:
##      Earl Hood       ehood@medusa.acs.uci.edu
##  Description:
##	Module to deal with ISO-8859 data.
##---------------------------------------------------------------------------##
##    Copyright (C) 1997        Earl Hood, ehood@medusa.acs.uci.edu
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
##    Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
##    02111-1307, USA
##---------------------------------------------------------------------------##

package SGML::ISO8859;

use Exporter;
@ISA = qw( Exporter );

$VERSION = "0.01";

@EXPORT_OK = qw(
    &str2sgml
    &sgml2str
);

##---------------------------------------------------------------------------
##      US-ASCII/Common characters
##---------------------------------------------------------------------------

%Char2Ent = (
  #--------------------------------------------------------------------------
  # Hex Code	Entity Ref	# ISO external entity and description
  #--------------------------------------------------------------------------
    0x26 =>	"amp",  	# ISOnum : Ampersand
    0x3C =>	"lt",   	# ISOnum : Less-than sign
    0x3E =>	"gt",   	# ISOnum : Greater-than sign

    0xA0 =>	"nbsp",  	# ISOnum : NO-BREAK SPACE
);

%Ent2Char = reverse %Char2Ent;

##---------------------------------------------------------------------------
##      Charset specification to mapping
##---------------------------------------------------------------------------

%CharsetSpec2Char2Ent = (
    'us-ascii'    =>	\%SGML::ISO8859::Char2Ent,
    'iso-8859-1'  =>	\%SGML::ISO8859::S1::Char2Ent,
    'iso-8859-2'  =>	\%SGML::ISO8859::S2::Char2Ent,
    'iso-8859-3'  =>	\%SGML::ISO8859::S3::Char2Ent,
    'iso-8859-4'  =>	\%SGML::ISO8859::S4::Char2Ent,
    'iso-8859-5'  =>	\%SGML::ISO8859::S5::Char2Ent,
    'iso-8859-6'  =>	\%SGML::ISO8859::S6::Char2Ent,
    'iso-8859-7'  =>	\%SGML::ISO8859::S7::Char2Ent,
    'iso-8859-8'  =>	\%SGML::ISO8859::S8::Char2Ent,
    'iso-8859-9'  =>	\%SGML::ISO8859::S9::Char2Ent,
    'iso-8859-10' =>	\%SGML::ISO8859::S10::Char2Ent,
);

%CharsetSpec2Ent2Char = (
    'us-ascii'    =>	\%SGML::ISO8859::Ent2Char,
    'iso-8859-1'  =>	\%SGML::ISO8859::S1::Ent2Char,
    'iso-8859-2'  =>	\%SGML::ISO8859::S2::Ent2Char,
    'iso-8859-3'  =>	\%SGML::ISO8859::S3::Ent2Char,
    'iso-8859-4'  =>	\%SGML::ISO8859::S4::Ent2Char,
    'iso-8859-5'  =>	\%SGML::ISO8859::S5::Ent2Char,
    'iso-8859-6'  =>	\%SGML::ISO8859::S6::Ent2Char,
    'iso-8859-7'  =>	\%SGML::ISO8859::S7::Ent2Char,
    'iso-8859-8'  =>	\%SGML::ISO8859::S8::Ent2Char,
    'iso-8859-9'  =>	\%SGML::ISO8859::S9::Ent2Char,
    'iso-8859-10' =>	\%SGML::ISO8859::S10::Ent2Char,
);

##---------------------------------------------------------------------------

###############################################################################
##	Routines
###############################################################################

##---------------------------------------------------------------------------##
##	str2sgml converts a string encoded by $charset to an sgml
##	string where special characters are converted to entity
##	references.
##
##	$return_data = SGML::ISO8859::str2sgml($data, $charset, $only8bit);
##
##	If $only8bit is non-zero, than only 8-bit characters are
##	translated.
##
sub str2sgml {
    my $data 	 =    shift;
    my $charset  = lc shift;
    my $only8bit =    shift;

    my($ret, $offset, $len) = ('', 0, 0);
    my($map);
    $charset =~ tr/_/-/;

    # Get mapping
    if ($charset =~ /iso-8859-(\d+)/) {
	$set = $1;
	require "SGML/ISO8859/S$set.pm";	# Load mapping
	$map = $CharsetSpec2Char2Ent{$charset};
    } else {
	$map = $CharsetSpec2Char2Ent{"us-ascii"};
    }

    # Convert string
    $len = length($data);
    while ($offset < $len) {
	$char = unpack("C", substr($data, $offset++, 1));
	if ($only8bit && $char < 0xA0) {
	    $ret .= pack("C", $char);
	} elsif ($map->{$char}) {
	    $ret .= join('', '&', $map->{$char}, ';');
	} elsif ($Char2Ent{$char}) {
	    $ret .= join('', '&', $Char2Ent{$char}, ';');
	}else {
	    $ret .= pack("C", $char);
	}
    }
    $ret;
}

##---------------------------------------------------------------------------##
##	sgml2str converts a string with sdata entity references to the
##	raw character values denoted by a character set.
##
##	$return_data = SGML::ISO8859::sgml2str($data, $charset);
##
sub sgml2str {
    my $data 	 =    shift;
    my $charset  = lc shift;

    my($map);
    $charset =~ tr/_/-/;

    # Get mapping
    if ($charset =~ /iso-8859-(\d+)/) {
	$set = $1;
	require "SGML/ISO8859/S$set.pm";	# Load mapping
	$map = $CharsetSpec2Ent2Char{$charset};
    } else {
	$map = $CharsetSpec2Ent2Char{"us-ascii"};
    }

    $data =~ s/\&([\w\.\-]+);
	      /defined($map->{$1}) ? sprintf("%c", $map->{$1}) :
		   defined($Ent2Char{$1}) ? sprintf("%c", $Ent2Char{$1}) :
		   "&$1;"
	      /gex;
    $data;
}

##---------------------------------------------------------------------------##
1;

