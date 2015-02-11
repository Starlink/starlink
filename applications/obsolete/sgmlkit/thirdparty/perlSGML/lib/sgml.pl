##---------------------------------------------------------------------------
##  File:
##	@(#)  sgml.pl 1.3 96/09/30 @(#)
##  Author:
##	Earl Hood, ehood@medusa.acs.uci.edu
##---------------------------------------------------------------------------
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

{
    package sgml;

    $VERSION = '1.0.0';

    $namechars   	= '\w.-';

    ##	Callback functions
    $EndTagFunc   	= '';
    $DeclFunc   	= '';
    $CommentFunc	= '';
    $CdataFunc   	= '';
    $OpenTagFunc   	= '';
    $ProcInsFunc	= '';
}

##---------------------------------------------------------------------------
##	SGMLread_sgml() reads SGML markup.  A callback is called when
##	the following occurs:
##
##	    o	An open tag:	&$OpenTagFunc($gi, $attribute_list)
##	    o	An end tag:	&$EndTagFunc($gi)
##	    o	A comment:	&$CommentFunc(*comment_text);
##	    o	Processing instruction:
##				&$ProcInsFunc(*pi_text);
##	    o	Character data: &$CdataFunc(*cdata);
##
##	Argument descriptions:
##	    $handle :	Filehandle containing the SGML instance.
##
##	Notes:
##	    o	read_sgml() is not intended to parse a DTD, or an
##		SGML declaration statement, '<!SGML ...>'.  It is
##		designed to parse SGML instances.  If a "<!" sequence
##		is encountered (and not part of a comment declaration,
##		read_sgml() tries to ignore the declaration.
##
##	    o	Marked sections are not recognized.
##
##	    o	The $CdataFunc may be called consective times for the
##		a contiguous character data segment.
##
sub SGMLread_sgml {
    package sgml;

    local($handle) = shift;
    local($data,
	  $tmp,
	  $char,
	  $txt,
	  $gi,
	  $left,
	  $oldhandle,
	  $oldds);

    $oldhandle = select($handle);
    $oldds = $/;
    $data = '';

    PSGML: while (!eof($handle)) {
	$txt = '';

	$/ = "<";
	$data = <$handle>;
	$left = chop $data;
	&$CdataFunc(*data)  if defined (&$CdataFunc);

	$char = getc($handle);

	if ($char eq '!') {			## Declaration
	    $char = getc($handle);
	    if ($char eq '-') {
		$char = getc($handle);
		while (1) {
		    $/ = ">";
		    $tmp = <$handle>;
		    last if $tmp =~ s/--\s*>$//o;
		    $txt .= $tmp;
		}
		$txt .= $tmp;
		&$CommentFunc(*txt)  if defined (&$CommentFunc);

	    } else {
		$/ = ">";
		$txt = <$handle>;
		chop $txt;
		&$DeclFunc(*txt)  if defined (&$DeclFunc);
	    }
	    next PSGML;
	}

	if ($char eq '?') {			## Processing instruction
	    $/ = ">";
	    $txt = <$handle>;
	    chop $txt;
	    &$ProcInsFunc(*txt)  if defined (&$ProcInsFunc);
	    next PSGML;
	}

	if ($char eq '/') {			## End tag
	    $/ = ">";
	    $txt = <$handle>;
	    $txt =~ s/[^$namechars]//go;
	    &$EndTagFunc($txt)  if defined (&$EndTagFunc);
	    next PSGML;
	}

	if ($char =~ /[$namechars]/o) { 	## Open tag
	    $gi = $char;
	    while (($char = getc($handle)) =~ /[$namechars]/o) {
		$gi .= $char;
	    }
	    if ($char ne '>') {
		while (!eof($handle)) {
		    $/ = ">";
		    $txt .= <$handle>;
		    last  if (&close_notin_lit($txt));
		}
		chop $txt;
	    }
	    &$OpenTagFunc($gi, $txt)  if defined (&$OpenTagFunc);
	    next PSGML;
	}

	&$CdataFunc(*left)  if defined (&$CdataFunc);

    }  ## End of parse loop

    $/ = $oldds;
    select($oldhandle);
}

##---------------------------------------------------------------------------##
##	Private functions
##---------------------------------------------------------------------------##

package sgml;

##----------------------------------------------------------------------
##	Function to check if string has a literla that is open.
##	The function returns 1 if it is not. Else it returns 0.
##
sub close_notin_lit {
    local($str) = ($_[0]);
    local($lit, $after);

    while ($str =~ /(['"])/) {
	$lit = $1;
	$after = $';
	if (($lit eq '"' ? ($after =~ /(")/) :
			   ($after =~ /(')/)) ) {
	    $str = $';
	} else {
	    return 0;
	}
    }
    1;
}

##---------------------------------------------------------------------------##
1;
