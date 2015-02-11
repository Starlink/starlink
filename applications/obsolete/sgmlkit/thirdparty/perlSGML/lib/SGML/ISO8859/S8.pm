##---------------------------------------------------------------------------##
##  File:
##      @(#) S8.pm 1.1 97/09/11 17:41:13 @(#)
##  Author:
##      Earl Hood       ehood@medusa.acs.uci.edu
##  Description:
##      Mappings for ISO-8859-8.
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

package SGML::ISO8859::S8;

##---------------------------------------------------------------------------
##      ISO-8859-8: Hebrew
##---------------------------------------------------------------------------
##	Note: There is no ISO entities for hebrew characters.  ISOamso
##	      defines a few characters, but they are for math purposes.
##	      Some of the following are non-standard entity references.
##	      "ISOhbrw" is used as the entity defining the Hebrew entities.

%Char2Ent = (
  #--------------------------------------------------------------------------
  # Hex Code	Entity Ref	# ISO external entity and description
  #--------------------------------------------------------------------------
    0xA2,	"cent", 	# ISOnum : CENT SIGN
    0xA3,	"pound",	# ISOnum : POUND SIGN
    0xA4,	"curren",	# ISOnum : CURRENCY SIGN
    0xA5,	"yen",  	# ISOnum : YEN SIGN
    0xA6,	"brvbar",	# ISOnum : BROKEN BAR
    0xA7,	"sect", 	# ISOnum : SECTION SIGN
    0xA8,	"die",  	# ISOdia : DIAERESIS
    0xA9,	"copy", 	# ISOnum : COPYRIGHT SIGN
    0xAA,	"times",	# ISOnum : MULTIPLICATION SIGN
    0xAB,	"laquo",	# ISOnum : LEFT-POINTING DOUBLE ANGLE
				#	   QUOTATION MARK
    0xAC,	"not",  	# ISOnum : NOT SIGN
    0xAD,	"shy",  	# ISOnum : SOFT HYPHEN
    0xAE,	"reg",  	# ISOnum : REGISTERED SIGN
    0xAF,	"macr", 	# ISOdia : OVERLINE (MACRON)
    0xB0,	"deg",  	# ISOnum : DEGREE SIGN
    0xB1,	"plusmn",	# ISOnum : PLUS-MINUS SIGN
    0xB2,	"sup2", 	# ISOnum : SUPERSCRIPT TWO
    0xB3,	"sup3", 	# ISOnum : SUPERSCRIPT THREE
    0xB4,	"acute",	# ISOdia : ACUTE ACCENT
    0xB5,	"micro",	# ISOnum : MICRO SIGN
    0xB6,	"para", 	# ISOnum : PILCROW SIGN
    0xB7,	"middot",	# ISOnum : MIDDLE DOT
    0xB8,	"cedil",	# ISOdia : CEDILLA
    0xB9,	"sup1", 	# ISOnum : SUPERSCRIPT ONE
    0xBA,	"divide",	# ISOlat1: DIVISION SIGN
    0xBB,	"raquo",	# ISOnum : RIGHT-POINTING DOUBLE ANGLE
				#	   QUOTATION MARK
    0xBC,	"frac14",	# ISOnum : VULGAR FRACTION ONE QUARTER
    0xBD,	"frac12", 	# ISOnum : VULGAR FRACTION ONE HALF
    0xBE,	"frac34",	# ISOnum : VULGAR FRACTION THREE QUARTERS
    0xDF,	"dlowbar",	# ISOnum?: DOUBLE LOW LINE
    0xE0,	"alef", 	# ISOhbrw: HEBREW LETTER ALEF
    0xE1,	"bet",  	# ISOhbrw: HEBREW LETTER BET
    0xE2,	"gimel",	# ISOhbrw: HEBREW LETTER GIMEL
    0xE3,	"dalet",	# ISOhbrw: HEBREW LETTER DALET
    0xE4,	"he",   	# ISOhbrw: HEBREW LETTER HE
    0xE5,	"vav",  	# ISOhbrw: HEBREW LETTER VAV
    0xE6,	"zayin",	# ISOhbrw: HEBREW LETTER ZAYIN
    0xE7,	"het",  	# ISOhbrw: HEBREW LETTER HET
    0xE8,	"tet",  	# ISOhbrw: HEBREW LETTER TET
    0xE9,	"yod",  	# ISOhbrw: HEBREW LETTER YOD
    0xEA,	"fkaf", 	# ISOhbrw: HEBREW LETTER FINAL KAF
    0xEB,	"kaf",  	# ISOhbrw: HEBREW LETTER KAF
    0xEC,	"lamed",	# ISOhbrw: HEBREW LETTER LAMED
    0xED,	"fmem", 	# ISOhbrw: HEBREW LETTER FINAL MEM
    0xEE,	"mem",  	# ISOhbrw: HEBREW LETTER MEM
    0xEF,	"fnun", 	# ISOhbrw: HEBREW LETTER FINAL NUN
    0xF0,	"nun",  	# ISOhbrw: HEBREW LETTER NUN
    0xF1,	"samekh",	# ISOhbrw: HEBREW LETTER SAMEKH
    0xF2,	"ayin", 	# ISOhbrw: HEBREW LETTER AYIN
    0xF3,	"fpe",  	# ISOhbrw: HEBREW LETTER FINAL PE
    0xF4,	"pe",   	# ISOhbrw: HEBREW LETTER PE
    0xF5,	"ftsadi",	# ISOhbrw: HEBREW LETTER FINAL TSADI
    0xF6,	"tsadi",	# ISOhbrw: HEBREW LETTER TSADI
    0xF7,	"qof",  	# ISOhbrw: HEBREW LETTER QOF
    0xF8,	"resh", 	# ISOhbrw: HEBREW LETTER RESH
    0xF9,	"shin", 	# ISOhbrw: HEBREW LETTER SHIN
    0xFA,	"tav",  	# ISOhbrw: HEBREW LETTER TAV
);

%Ent2Char = reverse %Char2Ent;
$Ent2Char{"half"}	= 0xBD; # ISOnum : VULGAR FRACTION ONE HALF

1;
