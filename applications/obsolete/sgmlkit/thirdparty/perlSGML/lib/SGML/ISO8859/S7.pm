##---------------------------------------------------------------------------##
##  File:
##      @(#) S7.pm 1.1 97/09/11 17:41:13 @(#)
##  Author:
##      Earl Hood       ehood@medusa.acs.uci.edu
##  Description:
##      Mappings for ISO-8859-7.
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

package SGML::ISO8859::S7;

##---------------------------------------------------------------------------
##      ISO-8859-7: Greek
##---------------------------------------------------------------------------

%Char2Ent = (
  #--------------------------------------------------------------------------
  # Hex Code	Entity Ref	# ISO external entity and description
  #--------------------------------------------------------------------------
    0xA1,	"lsquo",	# ISOnum : SINGLE HIGH-REVERSED-9 QUOTATION
				#	   MARK
    0xA2,	"rsquo",	# ISOnum : RIGHT SINGLE QUOTATION MARK
    0xA3,	"pound",	# ISOnum : POUND SIGN
    0xA6,	"brvbar",	# ISOnum : BROKEN BAR
    0xA7,	"sect", 	# ISOnum : SECTION SIGN
    0xA8,	"die",  	# ISOdia : DIAERESIS
    0xA9,	"copy", 	# ISOnum : COPYRIGHT SIGN
    0xAB,	"laquo",	# ISOnum : LEFT-POINTING DOUBLE ANGLE
				#	   QUOTATION MARK
    0xAC,	"not",  	# ISOnum : NOT SIGN
    0xAD,	"shy",  	# ISOnum : SOFT HYPHEN
    0xAF,	"mdash",	# ISOpub : EM DASH
    0xB0,	"deg",  	# ISOnum : DEGREE SIGN
    0xB1,	"plusmn",	# ISOnum : PLUS-MINUS SIGN
    0xB2,	"sup2", 	# ISOnum : SUPERSCRIPT TWO
    0xB3,	"sup3", 	# ISOnum : SUPERSCRIPT THREE
    0xB4,	"acute",	# ISOdia : ACUTE ACCENT
    0xB5,	"diagr",	# ISOgrk?: ACUTE ACCENT AND DIAERESIS
				#	   (Tonos and Dialytika)
    0xB6,	"Aacgr",	# ISOgrk2: GREEK CAPITAL LETTER ALPHA WITH
				#	   ACUTE
    0xB7,	"middot",	# ISOnum : MIDDLE DOT
    0xB8,	"Eacgr",	# ISOgrk2: GREEK CAPITAL LETTER EPSILON WITH
				#	   ACUTE
    0xB9,	"EEacgr",	# ISOgrk2: GREEK CAPITAL LETTER ETA WITH ACUTE
    0xBA,	"Iacgr",	# ISOgrk2: GREEK CAPITAL LETTER IOTA WITH ACUTE
    0xBB,	"raquo",	# ISOnum : RIGHT-POINTING DOUBLE ANGLE
				#	   QUOTATION MARK
    0xBC,	"Oacgr",	# ISOgrk2: GREEK CAPITAL LETTER OMICRON WITH
				#	   ACUTE
    0xBD,	"frac12", 	# ISOnum : VULGAR FRACTION ONE HALF
    0xBE,	"Uacgr",	# ISOgrk2: GREEK CAPITAL LETTER UPSILON WITH
				#	   ACUTE
    0xBF,	"OHacgr",	# ISOgrk2: GREEK CAPITAL LETTER OMEGA WITH
				#	   ACUTE
    0xC0,	"idiagr",	# ISOgrk2: GREEK SMALL LETTER IOTA WITH ACUTE
				#	   AND DIAERESIS
    0xC1,	"Agr",  	# ISOgrk1: GREEK CAPITAL LETTER ALPHA
    0xC2,	"Bgr",  	# ISOgrk1: GREEK CAPITAL LETTER BETA
    0xC3,	"Ggr",  	# ISOgrk1: GREEK CAPITAL LETTER GAMMA
    0xC4,	"Dgr",  	# ISOgrk1: GREEK CAPITAL LETTER DELTA
    0xC5,	"Egr",  	# ISOgrk1: GREEK CAPITAL LETTER EPSILON
    0xC6,	"Zgr",  	# ISOgrk1: GREEK CAPITAL LETTER ZETA
    0xC7,	"EEgr", 	# ISOgrk1: GREEK CAPITAL LETTER ETA
    0xC8,	"THgr", 	# ISOgrk1: GREEK CAPITAL LETTER THETA
    0xC9,	"Igr",  	# ISOgrk1: GREEK CAPITAL LETTER IOTA
    0xCA,	"Kgr",  	# ISOgrk1: GREEK CAPITAL LETTER KAPPA
    0xCB,	"Lgr",  	# ISOgrk1: GREEK CAPITAL LETTER LAMDA
    0xCC,	"Mgr",  	# ISOgrk1: GREEK CAPITAL LETTER MU
    0xCD,	"Ngr",  	# ISOgrk1: GREEK CAPITAL LETTER NU
    0xCE,	"Xgr",  	# ISOgrk1: GREEK CAPITAL LETTER XI
    0xCF,	"Ogr",  	# ISOgrk1: GREEK CAPITAL LETTER OMICRON
    0xD0,	"Pgr",  	# ISOgrk1: GREEK CAPITAL LETTER PI
    0xD1,	"Rgr",  	# ISOgrk1: GREEK CAPITAL LETTER RHO
    0xD3,	"Sgr",  	# ISOgrk1: GREEK CAPITAL LETTER SIGMA
    0xD4,	"Tgr",  	# ISOgrk1: GREEK CAPITAL LETTER TAU
    0xD5,	"Ugr",  	# ISOgrk1: GREEK CAPITAL LETTER UPSILON
    0xD6,	"PHgr", 	# ISOgrk1: GREEK CAPITAL LETTER PHI
    0xD7,	"KHgr", 	# ISOgrk1: GREEK CAPITAL LETTER CHI
    0xD8,	"PSgr", 	# ISOgrk1: GREEK CAPITAL LETTER PSI
    0xD9,	"OHgr", 	# ISOgrk1: GREEK CAPITAL LETTER OMEGA
    0xDA,	"Idigr",	# ISOgrk2: GREEK CAPITAL LETTER IOTA WITH
				#	   DIAERESIS
    0xDB,	"Udigr",	# ISOgrk2: GREEK CAPITAL LETTER UPSILON WITH
				#	   DIAERESIS
    0xDC,	"aacgr",	# ISOgrk2: GREEK SMALL LETTER ALPHA WITH ACUTE
    0xDD,	"eacgr",	# ISOgrk2: GREEK SMALL LETTER EPSILON WITH
				#	   ACUTE
    0xDE,	"eeacgr",	# ISOgrk2: GREEK SMALL LETTER ETA WITH ACUTE
    0xDF,	"iacgr",	# ISOgrk2: GREEK SMALL LETTER IOTA WITH ACUTE
    0xE0,	"udiagr",	# ISOgrk2: GREEK SMALL LETTER UPSILON WITH
				#	   ACUTE AND DIAERESIS
    0xE1,	"agr",  	# ISOgrk1: GREEK SMALL LETTER ALPHA
    0xE2,	"bgr",  	# ISOgrk1: GREEK SMALL LETTER BETA
    0xE3,	"ggr",  	# ISOgrk1: GREEK SMALL LETTER GAMMA
    0xE4,	"dgr",  	# ISOgrk1: GREEK SMALL LETTER DELTA
    0xE5,	"egr",  	# ISOgrk1: GREEK SMALL LETTER EPSILON
    0xE6,	"zgr",  	# ISOgrk1: GREEK SMALL LETTER ZETA
    0xE7,	"eegr", 	# ISOgrk1: GREEK SMALL LETTER ETA
    0xE8,	"thgr", 	# ISOgrk1: GREEK SMALL LETTER THETA
    0xE9,	"igr",  	# ISOgrk1: GREEK SMALL LETTER IOTA
    0xEA,	"kgr",  	# ISOgrk1: GREEK SMALL LETTER KAPPA
    0xEB,	"lgr",  	# ISOgrk1: GREEK SMALL LETTER LAMDA
    0xEC,	"mgr",  	# ISOgrk1: GREEK SMALL LETTER MU
    0xED,	"ngr",  	# ISOgrk1: GREEK SMALL LETTER NU
    0xEE,	"xgr",  	# ISOgrk1: GREEK SMALL LETTER XI
    0xEF,	"ogr",  	# ISOgrk1: GREEK SMALL LETTER OMICRON
    0xF0,	"pgr",  	# ISOgrk1: GREEK SMALL LETTER PI
    0xF1,	"rgr",  	# ISOgrk1: GREEK SMALL LETTER RHO
    0xF2,	"sfgr", 	# ISOgrk1: GREEK SMALL LETTER FINAL SIGMA
    0xF3,	"sgr",  	# ISOgrk1: GREEK SMALL LETTER SIGMA
    0xF4,	"tgr",  	# ISOgrk1: GREEK SMALL LETTER TAU
    0xF5,	"ugr",  	# ISOgrk1: GREEK SMALL LETTER UPSILON
    0xF6,	"phgr", 	# ISOgrk1: GREEK SMALL LETTER PHI
    0xF7,	"khgr", 	# ISOgrk1: GREEK SMALL LETTER CHI
    0xF8,	"psgr", 	# ISOgrk1: GREEK SMALL LETTER PSI
    0xF9,	"ohgr", 	# ISOgrk1: GREEK SMALL LETTER OMEGA
    0xFA,	"idigr",	# ISOgrk2: GREEK SMALL LETTER IOTA WITH
				#	   DIAERESIS
    0xFB,	"udigr",	# ISOgrk2: GREEK SMALL LETTER UPSILON WITH
				#	   DIAERESIS
    0xFC,	"oacgr",	# ISOgrk2: GREEK SMALL LETTER OMICRON WITH
				#	   ACUTE
    0xFD,	"uacgr",	# ISOgrk2: GREEK SMALL LETTER UPSILON WITH
				#	   ACUTE
    0xFE,	"ohacgr",	# ISOgrk2: GREEK SMALL LETTER OMEGA WITH ACUTE
);

%Ent2Char = reverse %Char2Ent;
$Ent2Char{"half"}	= 0xBD; # ISOnum : VULGAR FRACTION ONE HALF

1;
