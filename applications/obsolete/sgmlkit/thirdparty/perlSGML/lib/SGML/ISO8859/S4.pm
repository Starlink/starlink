##---------------------------------------------------------------------------##
##  File:
##      @(#) S4.pm 1.1 97/09/11 17:41:12 @(#)
##  Author:
##      Earl Hood       ehood@medusa.acs.uci.edu
##  Description:
##      Mappings for ISO-8859-4.
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

package SGML::ISO8859::S4;

##---------------------------------------------------------------------------
##	ISO-8859-4: Latin-4
##---------------------------------------------------------------------------

%Char2Ent = (
  #--------------------------------------------------------------------------
  # Hex Code	Entity Ref	# ISO external entity and description
  #--------------------------------------------------------------------------
    0xA1,	"Aogon",	# ISOlat2: LATIN CAPITAL LETTER A WITH OGONEK
    0xA2,	"kgreen",	# ISOlat2: LATIN SMALL LETTER KRA (Greenlandic)
    0xA3,	"Rcedil",	# ISOlat2: LATIN CAPITAL LETTER R WITH CEDILLA
    0xA4,	"curren",	# ISOnum : CURRENCY SIGN
    0xA5,	"Itilde",     	# ISOlat2: LATIN CAPITAL LETTER I WITH TILDE
    0xA6,	"Lcedil",	# ISOlat2: LATIN CAPITAL LETTER L WITH CEDILLA
    0xA7,	"sect", 	# ISOnum : SECTION SIGN
    0xA8,	"die",  	# ISOdia : DIAERESIS
    0xA9,	"Scaron",	# ISOlat2: LATIN CAPITAL LETTER S WITH CARON
    0xAA,	"Emacr",	# ISOlat2: LATIN CAPITAL LETTER E WITH MACRON
    0xAB,	"Gcedil",	# ISOlat2: LATIN CAPITAL LETTER G WITH CEDILLA
    0xAC,	"Tstrok",	# ISOlat2: LATIN CAPITAL LETTER T WITH STROKE
    0xAD,	"shy",  	# ISOnum : SOFT HYPHEN
    0xAE,	"Zcaron",     	# ISOlat2: LATIN CAPITAL LETTER Z WITH CARON
    0xAF,	"macr", 	# ISOdia : MACRON
    0xB0,	"deg",  	# ISOnum : DEGREE SIGN
    0xB1,	"aogon",	# ISOlat2: LATIN SMALL LETTER A WITH OGONEK
    0xB2,	"ogon", 	# ISOdia : OGONEK
    0xB3,	"rcedil",	# ISOlat2: LATIN SMALL LETTER R WITH CEDILLA
    0xB4,	"acute",	# ISOdia : ACUTE ACCENT
    0xB5,	"itilde",	# ISOlat2: LATIN SMALL LETTER I WITH TILDE
    0xB6,	"lcedil",	# ISOlat2: LATIN SMALL LETTER L WITH CEDILLA
    0xB7,	"caron",	# ISOdia : CARON
    0xB8,	"cedil",	# ISOdia : CEDILLA
    0xB9,	"scaron",	# ISOlat2: LATIN SMALL LETTER S WITH CARON
    0xBA,	"emacr",	# ISOlat2: LATIN SMALL LETTER E WITH MACRON
    0xBB,	"gcedil",	# ISOlat2: LATIN SMALL LETTER G WITH CEDILLA
    0xBC,	"tstrok",	# ISOlat2: LATIN SMALL LETTER J WITH STROKE
    0xBD,	"ENG",  	# ISOlat2: LATIN CAPITAL LETTER ENG (Lappish)
    0xBE,	"zcaron",    	# ISOlat2: LATIN SMALL LETTER Z WITH CARON
    0xBF,	"eng",  	# ISOlat2: LATIN SMALL LETTER ENG (Lappish)
    0xC0,	"Amacr",	# ISOlat1: LATIN CAPITAL LETTER A WITH MACRON
    0xC1,	"Aacute",	# ISOlat1: LATIN CAPITAL LETTER A WITH ACUTE
    0xC2,	"Acirc",	# ISOlat1: LATIN CAPITAL LETTER A WITH
				#	   CIRCUMFLEX
    0xC3,	"Atilde",     	# ISOlat1: LATIN CAPITAL LETTER A WITH TILDE
    0xC4,	"Auml", 	# ISOlat1: LATIN CAPITAL LETTER A WITH
				#	   DIAERESIS
    0xC5,	"Aring",	# ISOlat1: LATIN CAPITAL LETTER A WITH RING
				#	   ABOVE
    0xC6,	"AElig",	# ISOlat1: LATIN CAPITAL LETTER AE
    0xC7,	"Iogon",	# ISOlat2: LATIN CAPITAL LETTER I WITH OGONEK
    0xC8,	"Ccaron",	# ISOlat2: LATIN CAPITAL LETTER C WITH CARON
    0xC9,	"Eacute",	# ISOlat1: LATIN CAPITAL LETTER E WITH ACUTE
    0xCA,	"Eogon",	# ISOlat2: LATIN CAPITAL LETTER E WITH OGONEK
    0xCB,	"Euml", 	# ISOlat1: LATIN CAPITAL LETTER E WITH
				#	   DIAERESIS
    0xCC,	"Edot", 	# ISOlat1: LATIN CAPITAL LETTER E WITH DOT
				#	   ABOVE
    0xCD,	"Iacute",	# ISOlat1: LATIN CAPITAL LETTER I WITH ACUTE
    0xCE,	"Icirc",	# ISOlat1: LATIN CAPITAL LETTER I WITH
				#	   CIRCUMFLEX
    0xCF,	"Imacr",	# ISOlat2: LATIN CAPITAL LETTER I WITH MACRON
    0xD0,	"Dstrok",     	# ISOlat2: LATIN CAPITAL LETTER D WITH STROKE
    0xD1,	"Ncedil",	# ISOlat2: LATIN CAPITAL LETTER N WITH CEDILLA
    0xD2,	"Omacr",	# ISOlat2: LATIN CAPITAL LETTER O WITH MACRON
    0xD3,	"Kcedil",	# ISOlat2: LATIN CAPITAL LETTER K WITH CEDILLA
    0xD4,	"Ocirc",	# ISOlat1: LATIN CAPITAL LETTER O WITH
				#	   CIRCUMFLEX
    0xD5,	"Otilde",	# ISOlat1: LATIN CAPITAL LETTER O WITH TILDE
    0xD6,	"Ouml", 	# ISOlat1: LATIN CAPITAL LETTER O WITH
				#	   DIAERESIS
    0xD7,	"times",	# ISOnum : MULTIPLICATION SIGN
    0xD8,	"Oslash",	# ISOlat1: LATIN CAPITAL LETTER O WITH STROKE
    0xD9,	"Uogon",	# ISOlat2: LATIN CAPITAL LETTER U WITH OGONEK
    0xDA,	"Uacute",	# ISOlat1: LATIN CAPITAL LETTER U WITH ACUTE
    0xDB,	"Ucirc",	# ISOlat1: LATIN CAPITAL LETTER U WITH
				#	   CIRCUMFLEX
    0xDC,	"Uuml", 	# ISOlat1: LATIN CAPITAL LETTER U WITH
				#	   DIAERESIS
    0xDD,	"Utilde",	# ISOlat2: LATIN CAPITAL LETTER U WITH TILDE
    0xDE,	"Umacr",	# ISOlat2: LATIN CAPITAL LETTER U WITH MACRON
    0xDF,	"szlig",	# ISOlat1: LATIN SMALL LETTER SHARP S (German)
    0xE0,	"amacr",	# ISOlat1: LATIN SMALL LETTER A WITH MACRON
    0xE1,	"aacute",	# ISOlat1: LATIN SMALL LETTER A WITH ACUTE
    0xE2,	"acirc",	# ISOlat1: LATIN SMALL LETTER A WITH CIRCUMFLEX
    0xE3,	"atilde",     	# ISOlat1: LATIN SMALL LETTER A WITH TILDE
    0xE4,	"auml", 	# ISOlat1: LATIN SMALL LETTER A WITH DIAERESIS
    0xE5,	"aring",	# ISOlat1: LATIN SMALL LETTER A WITH RING ABOVE
    0xE6,	"aelig",	# ISOlat1: LATIN SMALL LETTER AE
    0xE7,	"iogon",	# ISOlat2: LATIN SMALL LETTER I WITH OGONEK
    0xE8,	"ccaron",	# ISOlat2: LATIN SMALL LETTER C WITH CARON
    0xE9,	"eacute",	# ISOlat2: LATIN SMALL LETTER E WITH ACUTE
    0xEA,	"eogon",	# ISOlat2: LATIN SMALL LETTER E WITH OGONEK
    0xEB,	"euml", 	# ISOlat1: LATIN SMALL LETTER E WITH DIAERESIS
    0xEC,	"edot", 	# ISOlat2: LATIN SMALL LETTER E WITH DOT ABOVE
    0xED,	"iacute",	# ISOlat1: LATIN SMALL LETTER I WITH ACUTE
    0xEE,	"icirc",	# ISOlat1: LATIN SMALL LETTER I WITH CIRCUMFLEX
    0xEF,	"imacr",	# ISOlat2: LATIN SMALL LETTER I WITH MACRON
    0xF0,	"dstrok",    	# ISOlat2: LATIN SMALL LETTER D WITH STROKE
    0xF1,	"ncedil",	# ISOlat2: LATIN SMALL LETTER N WITH CEDILLA
    0xF2,	"omacr",	# ISOlat2: LATIN SMALL LETTER O WITH MACRON
    0xF3,	"kcedil",	# ISOlat2: LATIN SMALL LETTER K WITH CEDILLA
    0xF4,	"ocirc",	# ISOlat1: LATIN SMALL LETTER O WITH CIRCUMFLEX
    0xF5,	"otilde",	# ISOlat1: LATIN SMALL LETTER O WITH TILDE
    0xF6,	"ouml", 	# ISOlat1: LATIN SMALL LETTER O WITH DIAERESIS
    0xF7,	"divide",	# ISOnum : DIVISION SIGN
    0xF8,	"oslash",	# ISOlat1: LATIN SMALL LETTER O WITH STROKE
    0xF9,	"uogon",	# ISOlat2: LATIN SMALL LETTER U WITH OGONEK
    0xFA,	"uacute",	# ISOlat1: LATIN SMALL LETTER U WITH ACUTE
    0xFB,	"ucirc",	# ISOlat1: LATIN SMALL LETTER U WITH
				#	   CIRCUMFLEX
    0xFC,	"uuml", 	# ISOlat1: LATIN SMALL LETTER U WITH DIAERESIS
    0xFD,	"utilde",	# ISOlat2: LATIN SMALL LETTER U WITH TILDE
    0xFE,	"umacr",	# ISOlat2: LATIN SMALL LETTER U WITH MACRON
    0xFF,	"dot",  	# ISOdia : DOT ABOVE
);

%Ent2Char = reverse %Char2Ent;

1;
