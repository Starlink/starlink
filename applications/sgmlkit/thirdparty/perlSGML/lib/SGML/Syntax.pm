##---------------------------------------------------------------------------##
##  File:
##      @(#)  Syntax.pm 1.5 97/09/15 @(#)
##  Author:
##      Earl Hood			ehood@medusa.acs.uci.edu
##  Description:
##      This file defines the SGML::Syntax class.
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

package SGML::Syntax;

use Exporter ();
@ISA = qw(Exporter);
$VERSION = "0.02";

%EXPORT_TAGS = (
    Delims	=> [
	qw( $mdo $mdo_ $mdc $mdc_ $mdo1char $mdo2char
	    $pio $pio_ $pic $pic_ $pio1char $pio2char
	    $stago $stago_ $etago $etago_ $tagc $tagc_
	    $msc $msc_
	    $rni $rni_
	    $vi $vi_
	    $ero $ero_ $pero $pero_ $cro $cro_ $refc $refc_
	    $dso $dso_ $dsc $dsc_
	    $comm $comm_ $como $como_ $comc $comc_ $comchar
	    $grpo $grpo_ $grpc $grpc_
	    $seq $seq_ $and $and_ $or $or_
	    $opt $opt_ $plus $plus_ $rep $rep_
	    $inc $inc_ $exc $exc_
	    $quotes $lit $lit_ $lita $lita_
	    $namechars $namestart
	  )
    ],

    Keywords	=> [
	qw( $ANY $ATTLIST $CDATA $COMMENT $CONREF $CURRENT
	    $DOCTYPE $ELEMENT $EMPTY $ENDTAG $ENTITY $ENTITIES
	    $FIXED $ID $IDREF $IDREFS $IGNORE $IMPLIED
	    $INCLUDE $LINK $LINKTYPE $MD $MS $NAME $NAMES
	    $NDATA $NMTOKEN $NMTOKENS $NOTATION $NUMBER
	    $NUMBERS $NUTOKEN $NUTOKENS $PCDATA $PI $PUBLIC
	    $RCDATA $REQUIRED $SDATA $SHORTREF $SIMPLE
	    $STARTTAG $SUBDOC $SYSTEM $TEMP $TEXT $USELINK
	    $USEMAP
	  )
    ],

    CharEnts	=> [
	qw( %CharEntity
	  )
    ],
);

@EXPORT = ();
@EXPORT_OK = ();

Exporter::export_ok_tags('Delims');
Exporter::export_ok_tags('Keywords');
Exporter::export_ok_tags('CharEnts');

##-------------------------##
## SGML key word variables ##
##-------------------------##
$ANY		= "ANY";
$ATTLIST	= "ATTLIST";
$CDATA		= "CDATA";
$COMMENT	= "--";
$CONREF		= "CONREF";
$CURRENT	= "CURRENT";
$DOCTYPE	= "DOCTYPE";
$ELEMENT	= "ELEMENT";
$EMPTY		= "EMPTY";
$ENDTAG		= "ENDTAG";
$ENTITY		= "ENTITY";
$ENTITIES	= "ENTITIES";
$FIXED		= "FIXED";
$ID		= "ID";
$IDREF		= "IDREF";
$IDREFS		= "IDREFS";
$IGNORE		= "IGNORE";
$IMPLIED	= "IMPLIED";
$INCLUDE	= "INCLUDE";
$LINK		= "LINK";
$LINKTYPE	= "LINKTYPE";
$MD		= "MD";
$MS		= "MS";
$NAME		= "NAME";
$NAMES		= "NAMES";
$NDATA		= "NDATA";
$NMTOKEN	= "NMTOKEN";
$NMTOKENS	= "NMTOKENS";
$NOTATION	= "NOTATION";
$NUMBER		= "NUMBER";
$NUMBERS	= "NUMBERS";
$NUTOKEN	= "NUTOKEN";
$NUTOKENS	= "NUTOKENS";
$PCDATA		= "PCDATA";
$PI		= "PI";
$PUBLIC		= "PUBLIC";
$RCDATA		= "RCDATA";
$REQUIRED	= "REQUIRED";
$SDATA		= "SDATA";
$SHORTREF	= "SHORTREF";
$SIMPLE		= "SIMPLE";
$STARTTAG	= "STARTTAG";
$SUBDOC		= "SUBDOC";
$SYSTEM		= "SYSTEM";
$TEMP		= "TEMP";
$TEXT		= "TEXT";
$USELINK	= "USELINK";
$USEMAP		= "USEMAP";

##------------------------------##
## SGML key character variables ##
##------------------------------##
## NOTE: Some variables have '\' characters because those variables are
##	 normally used in a Perl regular expression.  The variables 
##	 with the '_' appended to the end, are the non-escaped version
##	 of the variable.
##
## NOTE: If modifiy variables to support an alternative syntax, the
##	 first character of MDO and PIO must be the same.  The parsing
##	 routines for DTD.pm require this.  Also, MDO and PIO are
##	 assumed to be 2 characters in length (I know, strange, but the
##	 DTD parser code was written in '93 when I was still learning Perl,
##	 and I have yet to rewrite it).

$mdo	= '<!';		# Markup declaration open
$mdo_	= '<!';
$mdc	= '>';		# Markup declaration close
$mdc_	= '>';
$mdo1char = '<';	# This should also equal the first character in $pio
$mdo2char = '!';

$pio	= '<\?';	# Processing instruction open
$pio_	= '<?';
$pic	= '>';		# Processing instruction close
$pic_	= '>';
$pio1char = '<';
$pio2char = '?';

$stago	= '<';		# Start tag open
$stago_	= '<';
$etago	= '</';		# End tag open
$etago_	= '</';
$tagc	= '>';		# Tag close
$tagc_	= '>';

$vi	= '=';		# Value indicator
$vi_	= '=';

$msc	= '\]\]';	# Marked section close
$msc_	= ']]';

$rni	= '#';		# Reserved name indicator
$rni_	= '#';

$ero	= '&';		# General entity reference open
$ero_	= '&';
$pero	= '%';		# Parameter entity reference open
$pero_	= '%';
$cro	= '&#';		# Character reference open
$cro_	= '&#';
$refc	= ';';		# Reference close
$refc_	= ';';

$dso	= '\[';		# Doc type declaration subset open
$dso_	= '[';
$dsc	= '\]';		# Doc type declaration subset close
$dsc_	= ']';

## NOTE: It is not recommended to modify the comment delimiters.  The
##	 parsing routines require that the delimiters are 2 characters
##	 long, and the 2 characters are the same.

$comm	= '--';		# Comment
$comm_	= '--';
$como	= '--';		# Comment open
$como_	= '--';
$comc	= '--';		# Comment close (should be same as $como);
$comc_	= '--';
$comchar = '-';

$grpo	= '\(';		# Group open
$grpo_	= '(';
$grpc	= '\)';		# Group close
$grpc_	= ')';
$seq	= ',';		# Sequence connector
$seq_	= ',';
$and	= '&';		# And connector
$and_	= '&';
$or	= '\|';		# Or connector
$or_	= '|';
$opt	= '\?';		# Occurs zero or one time
$opt_	= '?';
$plus	= '\+';		# Occurs one or more times
$plus_	= '+';
$rep	= '\*';		# Occurs zero or more times
$rep_	= '*';
$inc	= '\+';		# Inclusion
$inc_	= '+';
$exc	= '-';		# Exclusion
$exc_	= '-';

$quotes	= q/'"/;	# Quote characters
$lit	= q/"/;
$lit_	= q/"/;
$lita	= q/'/;
$lita_	= q/'/;

##---------------------##
## SGML misc variables ##
##---------------------##
$namestart = 'A-Za-z';		# Regular expr repesenting characters that
				# can start a name token.
				# Should be used in '[]' in a regexp.

$namechars = 'A-Za-z0-9\.\-';	# Regular expr repesenting characters in a
				# name token.
				# Should be used in '[]' in a regexp.

%CharEntity = (		# Character entities
    'RE',	"\r",		# Record end
    'RS',	"\n",		# Record start
    'SPACE',	" ",		# Space
    'TAB',	"\t",		# Tab
    '34',	'"',		# Double quote
    '35',	'#',		# Number sign
    '37',	'%',		# Percent
    '39',	"'",		# Single quote
    '40',	'(',		# Left paren
    '41',	')',		# Right paren
    '42',	'*',		# Asterix
    '43',	'+',		# Plus
    '44',	',',		# Comma
    '45',	'-',		# Minus/hyphen
    '58',	':',		# Colon
    '59',	';',		# Semi-colon
    '61',	'=',		# Equal sign
    '64',	'@',		# At sign
    '91',	'[',		# Left square bracket
    '93',	']',		# Right square bracket
    '94',	'^',		# Carret
    '95',	'_',		# Underscore
    '123',	'{',		# Left curly brace
    '124',	'|',		# Vertical bar
    '125',	'}',		# Right curly brace
    '126',	'~',		# Tilde
);

1;
