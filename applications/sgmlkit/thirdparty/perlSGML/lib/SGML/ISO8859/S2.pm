##---------------------------------------------------------------------------##
##  File:
##      @(#) S2.pm 1.1 97/09/11 17:41:11 @(#)
##  Author:
##      Earl Hood       ehood@medusa.acs.uci.edu
##  Description:
##      Mappings for ISO-8859-2.
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

###############################################################################
##	Mapping arrays for characters to entity references
###############################################################################

package SGML::ISO8859::S2;

##---------------------------------------------------------------------------
##	ISO-8859-2: Latin-2
##---------------------------------------------------------------------------

%Char2Ent = (
  #--------------------------------------------------------------------------
  # Hex Code	Entity Ref	# ISO external entity and description
  #--------------------------------------------------------------------------
    0xA1,	"Aogon",	# ISOlat2: LATIN CAPITAL LETTER A WITH OGONEK
    0xA2,	"breve",	# ISOdia : BREVE
    0xA3,	"Lstrok",	# ISOlat2: LATIN CAPITAL LETTER L WITH STROKE
    0xA4,	"curren",	# ISOnum : CURRENCY SIGN
    0xA5,	"Lcaron",	# ISOlat2: LATIN CAPITAL LETTER L WITH CARON
    0xA6,	"Sacute",	# ISOlat2: LATIN CAPITAL LETTER S WITH ACUTE
    0xA7,	"sect",		# ISOnum : SECTION SIGN
    0xA8,	"die",		# ISOdia : DIAERESIS
    0xA9,	"Scaron",	# ISOlat2: LATIN CAPITAL LETTER S WITH CARON
    0xAA,	"Scedil",	# ISOlat2: LATIN CAPITAL LETTER S WITH CEDILLA
    0xAB,	"Tcaron",	# ISOlat2: LATIN CAPITAL LETTER T WITH CARON
    0xAC,	"Zacute",	# ISOlat2: LATIN CAPITAL LETTER Z WITH ACUTE
    0xAD,	"shy",		# ISOnum : SOFT HYPHEN
    0xAE,	"Zcaron",	# ISOlat2: LATIN CAPITAL LETTER Z WITH CARON
    0xAF,	"Zdot",		# ISOlat2: LATIN CAPITAL LETTER Z WITH DOT
				#	   ABOVE
    0xB0,	"deg",		# ISOnum : DEGREE SIGN
    0xB1,	"aogon",	# ISOlat2: LATIN SMALL LETTER A WITH OGONEK
    0xB2,	"ogon",		# ISOdia : OGONEK
    0xB3,	"lstrok",	# ISOlat2: LATIN SMALL LETTER L WITH STROKE
    0xB4,	"acute",	# ISOdia : ACUTE ACCENT
    0xB5,	"lcaron",	# ISOlat2: LATIN SMALL LETTER L WITH CARON
    0xB6,	"sacute",	# ISOlat2: LATIN SMALL LETTER S WITH ACUTE
    0xB7,	"caron",	# ISOdia : CARON
    0xB8,	"cedil",	# ISOdia : CEDILLA
    0xB9,	"scaron",	# ISOlat2: LATIN SMALL LETTER S WITH CARON
    0xBA,	"scedil",	# ISOlat2: LATIN SMALL LETTER S WITH CEDILLA
    0xBB,	"tcaron",	# ISOlat2: LATIN SMALL LETTER T WITH CARON
    0xBC,	"zacute",	# ISOlat2: LATIN SMALL LETTER Z WITH ACUTE
    0xBD,	"dblac",	# ISOdia : DOUBLE ACUTE ACCENT
    0xBE,	"zcaron",	# ISOlat2: LATIN SMALL LETTER Z WITH CARON
    0xBF,	"zdot",		# ISOlat2: LATIN SMALL LETTER Z WITH DOT ABOVE
    0xC0,	"Racute",	# ISOlat2: LATIN CAPITAL LETTER R WITH ACUTE
    0xC1,	"Aacute",	# ISOlat1: LATIN CAPITAL LETTER A WITH ACUTE
    0xC2,	"Acirc",	# ISOlat1: LATIN CAPITAL LETTER A WITH
				#	   CIRCUMFLEX
    0xC3,	"Abreve",	# ISOlat2: LATIN CAPITAL LETTER A WITH BREVE
    0xC4,	"Auml",		# ISOlat1: LATIN CAPITAL LETTER A WITH
				#	   DIAERESIS
    0xC5,	"Lacute",	# ISOlat2: LATIN CAPITAL LETTER L WITH ACUTE
    0xC6,	"Cacute",	# ISOlat2: LATIN CAPITAL LETTER C WITH ACUTE
    0xC7,	"Ccedil",	# ISOlat2: LATIN CAPITAL LETTER C WITH CEDILLA
    0xC8,	"Ccaron",	# ISOlat2: LATIN CAPITAL LETTER C WITH CARON
    0xC9,	"Eacute",	# ISOlat1: LATIN CAPITAL LETTER E WITH ACUTE
    0xCA,	"Eogon",	# ISOlat2: LATIN CAPITAL LETTER E WITH OGONEK
    0xCB,	"Euml",		# ISOlat1: LATIN CAPITAL LETTER E WITH
				#	   DIAERESIS
    0xCC,	"Ecaron",	# ISOlat2: LATIN CAPITAL LETTER E WITH CARON
    0xCD,	"Iacute",	# ISOlat1: LATIN CAPITAL LETTER I WITH ACUTE
    0xCE,	"Icirc",	# ISOlat1: LATIN CAPITAL LETTER I WITH
				#	   CIRCUMFLEX
    0xCF,	"Dcaron",	# ISOlat2: LATIN CAPITAL LETTER D WITH CARON
    0xD0,	"Dstrok",	# ISOlat2: LATIN CAPITAL LETTER D WITH STROKE
    0xD1,	"Nacute",	# ISOlat2: LATIN CAPITAL LETTER N WITH ACUTE
    0xD2,	"Ncaron",	# ISOlat2: LATIN CAPITAL LETTER N WITH CARON
    0xD3,	"Oacute",	# ISOlat1: LATIN CAPITAL LETTER O WITH ACUTE
    0xD4,	"Ocirc",	# ISOlat1: LATIN CAPITAL LETTER O WITH
				#	   CIRCUMFLEX
    0xD5,	"Odblac",	# ISOlat2: LATIN CAPITAL LETTER O WITH DOUBLE
				#	   ACUTE
    0xD6,	"Ouml",		# ISOlat1: LATIN CAPITAL LETTER O WITH
				#	   DIAERESIS
    0xD7,	"times",	# ISOnum : MULTIPLICATION SIGN
    0xD8,	"Rcaron",	# ISOlat2: LATIN CAPITAL LETTER R WITH CARON
    0xD9,	"Uring",	# ISOlat2: LATIN CAPITAL LETTER U WITH RING
				#	   ABOVE
    0xDA,	"Uacute",	# ISOlat1: LATIN CAPITAL LETTER U WITH ACUTE
    0xDB,	"Udblac",	# ISOlat2: LATIN CAPITAL LETTER U WITH DOUBLE
				#	   ACUTE
    0xDC,	"Uuml",		# ISOlat1: LATIN CAPITAL LETTER U WITH
				#	   DIAERESIS
    0xDD,	"Yacute",	# ISOlat2: LATIN CAPITAL LETTER Y WITH ACUTE
    0xDE,	"Tcedil",	# ISOlat2: LATIN CAPITAL LETTER T WITH CEDILLA
    0xDF,	"szlig",	# ISOlat1: LATIN SMALL LETTER SHARP S (German)
    0xE0,	"racute",	# ISOlat2: LATIN SMALL LETTER R WITH ACUTE
    0xE1,	"aacute",	# ISOlat1: LATIN SMALL LETTER A WITH ACUTE
    0xE2,	"acirc",	# ISOlat1: LATIN SMALL LETTER A WITH CIRCUMFLEX
    0xE3,	"abreve",	# ISOlat2: LATIN SMALL LETTER A WITH BREVE
    0xE4,	"auml",		# ISOlat1: LATIN SMALL LETTER A WITH DIAERESIS
    0xE5,	"lacute",	# ISOlat2: LATIN SMALL LETTER L WITH ACUTE
    0xE6,	"cacute",	# ISOlat2: LATIN SMALL LETTER C WITH ACUTE
    0xE7,	"ccedil",	# ISOlat1: LATIN SMALL LETTER C WITH CEDILLA
    0xE8,	"ccaron",	# ISOlat2: LATIN SMALL LETTER C WITH CARON
    0xE9,	"eacute",	# ISOlat1: LATIN SMALL LETTER E WITH ACUTE
    0xEA,	"eogon",	# ISOlat2: LATIN SMALL LETTER E WITH OGONEK
    0xEB,	"euml",		# ISOlat1: LATIN SMALL LETTER E WITH DIAERESIS
    0xEC,	"ecaron",	# ISOlat2: LATIN SMALL LETTER E WITH CARON
    0xED,	"iacute",	# ISOlat1: LATIN SMALL LETTER I WITH ACUTE
    0xEE,	"icirc",	# ISOlat1: LATIN SMALL LETTER I WITH CIRCUMFLEX
    0xEF,	"dcaron",	# ISOlat2: LATIN SMALL LETTER D WITH CARON
    0xF0,	"dstrok",	# ISOlat2: LATIN SMALL LETTER D WITH STROKE
    0xF1,	"nacute",	# ISOlat2: LATIN SMALL LETTER N WITH ACUTE
    0xF2,	"ncaron",	# ISOlat2: LATIN SMALL LETTER N WITH CARON
    0xF3,	"oacute",	# ISOlat1: LATIN SMALL LETTER O WITH ACUTE
    0xF4,	"ocirc",	# ISOlat1: LATIN SMALL LETTER O WITH CIRCUMFLEX
    0xF5,	"odblac",	# ISOlat2: LATIN SMALL LETTER O WITH DOUBLE
				#	   ACUTE
    0xF6,	"ouml",		# ISOlat1: LATIN SMALL LETTER O WITH DIAERESIS
    0xF7,	"divide",	# ISOnum : DIVISION SIGN
    0xF8,	"rcaron",	# ISOlat2: LATIN SMALL LETTER R WITH CARON
    0xF9,	"uring",	# ISOlat2: LATIN SMALL LETTER U WITH RING ABOVE
    0xFA,	"uacute",	# ISOlat1: LATIN SMALL LETTER U WITH ACUTE
    0xFB,	"udblac",	# ISOlat2: LATIN SMALL LETTER U WITH DOUBLE
				#	   ACUTE
    0xFC,	"uuml",		# ISOlat1: LATIN SMALL LETTER U WITH DIAERESIS
    0xFD,	"yacute",	# ISOlat1: LATIN SMALL LETTER Y WITH ACUTE
    0xFE,	"tcedil",	# ISOlat2: LATIN SMALL LETTER T WITH CEDILLA
    0xFF,	"dot",		# ISOdia : DOT ABOVE
);

%Ent2Char = reverse %Char2Ent;

1;
