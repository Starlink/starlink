##---------------------------------------------------------------------------##
##  File:
##      @(#) S3.pm 1.1 97/09/11 17:41:11 @(#)
##  Author:
##      Earl Hood       ehood@medusa.acs.uci.edu
##  Description:
##      Mappings for ISO-8859-3.
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

package SGML::ISO8859::S3;

##---------------------------------------------------------------------------
##	ISO-8859-3: Latin-3
##---------------------------------------------------------------------------

%Char2Ent = (
  #--------------------------------------------------------------------------
  # Hex Code	Entity Ref	# ISO external entity and description
  #--------------------------------------------------------------------------
    0xA1,	"Hstrok",	# ISOlat2: LATIN CAPITAL LETTER H WITH STROKE
    0xA2,	"breve",	# ISOdia : BREVE
    0xA3,	"pound",	# ISOnum : POUND SIGN
    0xA4,	"curren",	# ISOnum : CURRENCY SIGN
    0xA6,	"Hcirc",	# ISOlat2: LATIN CAPITAL LETTER H WITH
				#	   CIRCUMFLEX
    0xA7,	"sect", 	# ISOnum : SECTION SIGN
    0xA8,	"die",  	# ISOdia : DIAERESIS
    0xA9,	"Idot", 	# ISOlat2: LATIN CAPITAL LETTER I WITH DOT
				#	   ABOVE
    0xAA,	"Scedil",	# ISOlat2: LATIN CAPITAL LETTER S WITH CEDILLA
    0xAB,	"Gbreve",	# ISOlat2: LATIN CAPITAL LETTER G WITH BREVE
    0xAC,	"Jcirc",	# ISOlat2: LATIN CAPITAL LETTER J WITH
				#	   CIRCUMFLEX
    0xAD,	"shy",  	# ISOnum : SOFT HYPHEN
    0xAF,	"Zdot", 	# ISOlat2: LATIN CAPITAL LETTER Z WITH DOT
				#	   ABOVE
    0xB0,	"deg",  	# ISOnum : DEGREE SIGN
    0xB1,	"hstrok",	# ISOlat2: LATIN SMALL LETTER H WITH STROKE
    0xB2,	"sup2", 	# ISOnum : SUPERSCRIPT TWO
    0xB3,	"sup3", 	# ISOnum : SUPERSCRIPT THREE
    0xB4,	"acute",	# ISOdia : ACUTE ACCENT
    0xB5,	"micro",	# ISOnum : MICRO SIGN
    0xB6,	"hcirc",	# ISOlat2: LATIN SMALL LETTER H WITH
				#	   CIRCUMFLEX
    0xB7,	"middot",	# ISOnum : MIDDLE DOT
    0xB8,	"cedil",	# ISOdia : CEDILLA
    0xB9,	"inodot",	# ISOlat2: LATIN SMALL LETTER I DOTLESS
    0xBA,	"scedil",	# ISOlat2: LATIN SMALL LETTER S WITH CEDILLA
    0xBB,	"gbreve",	# ISOlat2: LATIN SMALL LETTER G WITH BREVE
    0xBC,	"jcirc",	# ISOlat2: LATIN SMALL LETTER J WITH CIRCUMFLEX
    0xBD,	"frac12", 	# ISOnum : VULGAR FRACTION ONE HALF
    0xBF,	"zdot", 	# ISOlat2: LATIN SMALL LETTER Z WITH DOT ABOVE
    0xC0,	"Agrave",	# ISOlat1: LATIN CAPITAL LETTER A WITH GRAVE
    0xC1,	"Aacute",	# ISOlat1: LATIN CAPITAL LETTER A WITH ACUTE
    0xC2,	"Acirc",	# ISOlat1: LATIN CAPITAL LETTER A WITH
				#	   CIRCUMFLEX
    0xC4,	"Auml", 	# ISOlat1: LATIN CAPITAL LETTER A WITH
				#	   DIAERESIS
    0xC5,	"Cdot", 	# ISOlat2: LATIN CAPITAL LETTER C WITH DOT
				#	   ABOVE
    0xC6,	"Ccirc",	# ISOlat2: LATIN CAPITAL LETTER C WITH
				#	   CIRCUMFLEX
    0xC7,	"Ccedil",	# ISOlat2: LATIN CAPITAL LETTER C WITH CEDILLA
    0xC8,	"Egrave",	# ISOlat1: LATIN CAPITAL LETTER E WITH GRAVE
    0xC9,	"Eacute",	# ISOlat1: LATIN CAPITAL LETTER E WITH ACUTE
    0xCA,	"Ecirc",	# ISOlat2: LATIN CAPITAL LETTER E WITH
				#	   CIRCUMFLEX
    0xCB,	"Euml", 	# ISOlat1: LATIN CAPITAL LETTER E WITH
				#	   DIAERESIS
    0xCC,	"Igrave",	# ISOlat1: LATIN CAPITAL LETTER I WITH GRAVE
    0xCD,	"Iacute",	# ISOlat1: LATIN CAPITAL LETTER I WITH ACUTE
    0xCE,	"Icirc",	# ISOlat1: LATIN CAPITAL LETTER I WITH
				#	   CIRCUMFLEX
    0xCF,	"Iuml", 	# ISOlat1: LATIN CAPITAL LETTER I WITH
				#	   DIAERESIS
    0xD1,	"Ntilde",	# ISOlat1: LATIN CAPITAL LETTER N WITH TILDE
    0xD2,	"Ograve",	# ISOlat1: LATIN CAPITAL LETTER O WITH GRAVE
    0xD3,	"Oacute",	# ISOlat1: LATIN CAPITAL LETTER O WITH ACUTE
    0xD4,	"Ocirc",	# ISOlat1: LATIN CAPITAL LETTER O WITH
				#	   CIRCUMFLEX
    0xD5,	"Gdot", 	# ISOlat2: LATIN CAPITAL LETTER G WITH DOT
				#	   ABOVE
    0xD6,	"Ouml", 	# ISOlat1: LATIN CAPITAL LETTER O WITH
				#	   DIAERESIS
    0xD7,	"times",	# ISOnum : MULTIPLICATION SIGN
    0xD8,	"Gcirc",	# ISOlat2: LATIN CAPITAL LETTER G WITH
				#	   CIRCUMFLEX
    0xD9,	"Ugrave",	# ISOlat1: LATIN CAPITAL LETTER U WITH GRAVE
				#	   ABOVE
    0xDA,	"Uacute",	# ISOlat1: LATIN CAPITAL LETTER U WITH ACUTE
    0xDB,	"Ucirc",	# ISOlat1: LATIN CAPITAL LETTER U WITH
				#	   CIRCUMFLEX
    0xDC,	"Uuml", 	# ISOlat1: LATIN CAPITAL LETTER U WITH
				#	   DIAERESIS
    0xDD,	"Ubreve",	# ISOlat2: LATIN CAPITAL LETTER U WITH BREVE
    0xDE,	"Scirc",	# ISOlat2: LATIN CAPITAL LETTER S WITH
				#	   CIRCUMFLEX
    0xDF,	"szlig",	# ISOlat1: LATIN SMALL LETTER SHARP S (German)
    0xE0,	"agrave",	# ISOlat1: LATIN SMALL LETTER A WITH GRAVE
    0xE1,	"aacute",	# ISOlat1: LATIN SMALL LETTER A WITH ACUTE
    0xE2,	"acirc",	# ISOlat1: LATIN SMALL LETTER A WITH CIRCUMFLEX
    0xE4,	"auml", 	# ISOlat1: LATIN SMALL LETTER A WITH DIAERESIS
    0xE5,	"cdot", 	# ISOlat2: LATIN SMALL LETTER C WITH DOT ABOVE
    0xE6,	"ccirce",	# ISOlat2: LATIN SMALL LETTER C WITH
				#	   CIRCUMFLEX
    0xE7,	"ccedil",	# ISOlat1: LATIN SMALL LETTER C WITH CEDILLA
    0xE8,	"egrave",	# ISOlat1: LATIN SMALL LETTER E WITH GRAVE
    0xE9,	"eacute",	# ISOlat2: LATIN SMALL LETTER E WITH ACUTE
    0xEA,	"ecirc",	# ISOlat2: LATIN SMALL LETTER E WITH
				#	   CIRCUMFLEX
    0xEB,	"euml", 	# ISOlat1: LATIN SMALL LETTER E WITH DIAERESIS
    0xEC,	"igrave",	# ISOlat1: LATIN SMALL LETTER I WITH GRAVE
    0xED,	"iacute",	# ISOlat1: LATIN SMALL LETTER I WITH ACUTE
    0xEE,	"icirc",	# ISOlat1: LATIN SMALL LETTER I WITH CIRCUMFLEX
    0xEF,	"iuml", 	# ISOlat1: LATIN SMALL LETTER I WITH DIAERESIS
    0xF1,	"ntilde",	# ISOlat1: LATIN SMALL LETTER N WITH TILDE
    0xF2,	"ograve",	# ISOlat1: LATIN SMALL LETTER O WITH GRAVE
    0xF3,	"oacute",	# ISOlat1: LATIN SMALL LETTER O WITH ACUTE
    0xF4,	"ocirc",	# ISOlat1: LATIN SMALL LETTER O WITH CIRCUMFLEX
    0xF5,	"gdot", 	# ISOlat2: LATIN SMALL LETTER G WITH DOT ABOVE
    0xF6,	"ouml", 	# ISOlat1: LATIN SMALL LETTER O WITH DIAERESIS
    0xF7,	"divide",	# ISOnum : DIVISION SIGN
    0xF8,	"gcirc",	# ISOlat2: LATIN SMALL LETTER G WITH
				#	   CIRCUMFLEX
    0xF9,	"ugrave",	# ISOlat1: LATIN SMALL LETTER U WITH GRAVE
    0xFA,	"uacute",	# ISOlat1: LATIN SMALL LETTER U WITH ACUTE
    0xFB,	"ucirc",	# ISOlat1: LATIN SMALL LETTER U WITH
				#	   CIRCUMFLEX
    0xFC,	"uuml", 	# ISOlat1: LATIN SMALL LETTER U WITH DIAERESIS
    0xFD,	"ubreve",	# ISOlat2: LATIN SMALL LETTER U WITH BREVE
    0xFE,	"scirc",	# ISOlat2: LATIN SMALL LETTER S WITH
				#	   CIRCUMFLEX
    0xFF,	"dot",  	# ISOdia : DOT ABOVE
);

%Ent2Char = reverse %Char2Ent;
$Ent2Char{"half"}	= 0xBD; # ISOnum : VULGAR FRACTION ONE HALF

1;
