##---------------------------------------------------------------------------##
##  File:
##      @(#)  Util.pm 1.8 97/09/15 14:58:26 @(#)
##  Author:
##      Earl Hood			ehood@medusa.acs.uci.edu
##  Description:
##      This file defines the SGML::Util module.  Module contains
##	utility routines for SGML processing.
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

package SGML::Util;

use SGML::Syntax qw(:Delims);

## Derive from Exporter
use Exporter ();
@ISA = qw(Exporter);

@EXPORT = ();
@EXPORT_OK = ();
%EXPORT_TAGS = (
    Routines => [
	qw( &SGMLparse_attr_spec
	    &SGMLattr_to_sgml
	    &SGMLopen_lit
	  )
    ],
);
$VERSION = "0.04";

Exporter::export_tags('Routines');

##---------------------------------------------------------------------------##
##	SGMLparse_attr_spec parses an attribute specification list
##	into name/value pairs.
##
##	Parameters:
##	    $	: A scalar string representing the SGML attribute
##		  specificaion list.
##
##	Return:
##	    @	: An array of name value pairs.  The calling routine
##		  can assign the return value to a hash to allow
##		  easy access to attribute values.  The name/value
##		  pairs occur in the same order as listed in the
##		  specification list.
##
##	Notes:
##	    o   The stago, gi, and etago should NOT be in the
##		specification list string.
##
##	    o	All attribute names are converted to lowercase.
##
##	    o   Attribute values w/o a name are given a bogus name
##		of the reserved name indicator ('#' in the reference
##		concrete syntax) with a number appended (eg. "#4").
##		This is to handle the case when SHORTTAG is YES.
##
##	    o   Any non-whitespace character is treated as a name
##		character.  This allows the parsing of SGML-like
##		markup.  For example, the following will not generate
##		a complaint:
##
##			  % = 100
##			  width = 100%
##
sub SGMLparse_attr_spec {
    my $spec = shift;
    my($str, $var, $q);
    my(@ret) = ();
    my $n = 0;

    ## Remove beginning whitespace
    ($str = $spec) =~ s/^\s+//;

    LOOP: while (1) {

	## Check for name=value specification
	while ($str =~ /^([^$vi\s]+)\s*$vi\s*/o) {
	    $var = lc $1;
	    $str = $';
	    if ($str =~ s/^([$quotes])//) {
		$q = $1;
		if (!($q eq $lit_ ? $str =~ s/^([^$lit]*)$lit//o :
				    $str =~ s/^([^$lita]*)$lita//o)) {
		    warn "Warning: Unclosed literal in: $spec\n";
		    push(@ret, $var, $str);
		    last LOOP;
		}
		$value = $1;
	    } else {
		if ($str =~ s/^(\S+)//) {
		    $value = $1;
		} else {
		    warn "Warning: No value after $var in: $spec\n";
		    last LOOP;
		}
	    }
	    $str =~ s/^\s+//;
	    push(@ret, $var, $value);
	}

	## Check if just value specified
	if ($str =~ s/^([$quotes])//) {		# Literal value
	    $q = $1;
	    if (!($q eq $lit_ ? $str =~ s/^([^$lit]*)$lit//o :
				$str =~ s/^([^$lita]*)$lita//o)) {
		warn "Warning: Unclosed literal in: $spec\n";
		push(@ret, sprintf("$rni_%05d", $n++), $str);
		last LOOP;
	    }
	    push(@ret, sprintf("$rni_%05d", $n++), $1);
	    next LOOP;
	}
	if ($str =~ s/^(\S+)\s*//o) {		# Name value
	    push(@ret, sprintf("$rni_%05d", $n++), $1);
	    next LOOP;
	}

	## Probably should never get here
	if ($str =~ /\S/) {
	    warn "Warning: Illegal attribute specification syntax in: ",
		 "$spec\n";
	}
	last LOOP;
    }

    @ret;
}

##---------------------------------------------------------------------------##
##	SGMLattr_to_sgml is the inverse operation of SGMLparse_attr_spec.
##	It takes a attribute structure and generates the SGML markup
##	representation.
##
##	Parameters:
##	    $	: A reference to a hash or an array.  If a hash, the
##		  keys represent the names and the values the attribute
##		  values.  If an array, the array is interpreted as
##		  a sequence of name/value pairs.
##
##	Return:
##	    $	: A string containing the SGML representation of the
##		  attributes.
##
##	Notes:
##	    o	Attribute names starting with the reserved name indicator
##		('#' in the reference concrete syntax) are skipped with
##		only their values printend.  This is to handle the case
##		when SHORTTAG is YES.
##
sub SGMLattr_to_sgml {
    my $ref = shift;
    my $str = '';
    my($name, $value, $q);

    ## If reference to hash, change to an array
    if (ref($ref) eq 'HASH') {
	my @a;
	foreach (sort keys %$ref) {	# Should we sort?
	   push(@a, $_, $ref->{$_});
	}
	$ref = \@a;

    ## If already an array, copy it
    } else {
	$ref = [ @$ref ];
    }

    while (@$ref) {
	$name = shift @$ref;
	$value = shift @$ref;
	if ($name !~ /$rni/o) {		# Check if printable name
	    $str .= "$name$vi_";
	    $q = ($value =~ /$lit/o) ? $lita_ : $lit_;
	} else {			# Naked values are not quoted
	    $q = '';
	}
	$str .= "$q$value$q ";
    }
    chop $str;	# remove added space
    $str;
}

##----------------------------------------------------------------------
##	SGMLopen_lit checks if a string has a literal that is not
##	closed.  I.e. If there is a quote without a matching quote,
##	the routine will return true.
##
##	Parameters
##	    $	:  Scalar string to check
##
##	Return:
##	    $	:  1 if open literal, else 0.
##
sub SGMLopen_lit {
    my $str = $_[0];
    my($q, $after);

    while ($str =~ /([$quotes])/o) {
	$q = $1;
	$after = $';
	if (($q eq $lit_ ? ($after =~ /($lit)/o) :
			   ($after =~ /($lita)/o)) ) {
	    $str = $';
	} else {
	    return 1;
	}
    }
    0;
}

##---------------------------------------------------------------------------##
1;

