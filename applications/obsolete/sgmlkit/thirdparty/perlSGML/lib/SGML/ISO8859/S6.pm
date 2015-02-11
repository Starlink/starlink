##---------------------------------------------------------------------------##
##  File:
##      @(#) S6.pm 1.1 97/09/11 17:41:12 @(#)
##  Author:
##      Earl Hood       ehood@medusa.acs.uci.edu
##  Description:
##      Mappings for ISO-8859-6.
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

package SGML::ISO8859::S6;

##---------------------------------------------------------------------------
##      ISO-8859-6: Arabic
##---------------------------------------------------------------------------
##	Note: There is no ISO entities for arabic characters.  Some of
##	      the following are non-standard entity references.  "ISOarbc"
##	      is used as the entity defining the Arabic entities.

%Char2Ent = (
  #--------------------------------------------------------------------------
  # Hex Code	Entity Ref	# ISO external entity and description
  #--------------------------------------------------------------------------
    0xA4,	"curren",	# ISOnum : CURRENCY SIGN
    0xAC,	"arcomma",	# ISOarbc: ARABIC COMMA
    0xAD,	"shy",  	# ISOnum : SOFT HYPHEN
    0xBB,	"arsemi",	# ISOarbc: ARABIC SEMICOLON
    0xBF,	"arquest",	# ISOarbc: ARABIC QUESTION MARK
    0xC1,	"hamz", 	# ISOarbc: ARABIC LETTER HAMZA
    0xC2,	"alefmadd",	# ISOarbc: ARABIC LETTER ALEF WITH MADDA ABOVE
    0xC3,	"alefhamz",	# ISOarbc: ARABIC LETTER ALEF WITH HAMZA ABOVE
    0xC4,	"wawhamz",	# ISOarbc: ARABIC LETTER WAW WITH HAMZA ABOVE
    0xC5,	"alefhamz",	# ISOarbc: ARABIC LETTER ALEF WITH HAMZA BELOW
    0xC6,	"yehhamz",	# ISOarbc: ARABIC LETTER YEH WITH HAMZA ABOVE
    0xC7,	"alef", 	# ISOarbc: ARABIC LETTER ALEF
    0xC8,	"beh",  	# ISOarbc: ARABIC LETTER BEH
    0xC9,	"tehmarb",	# ISOarbc: ARABIC LETTER TEH MARBUTA
    0xCA,	"teh",  	# ISOarbc: ARABIC LETTER TEH
    0xCB,	"theh", 	# ISOarbc: ARABIC LETTER THEH
    0xCC,	"jeem", 	# ISOarbc: ARABIC LETTER JEEM
    0xCD,	"hah",  	# ISOarbc: ARABIC LETTER HAH
    0xCE,	"khah", 	# ISOarbc: ARABIC LETTER KHAH
    0xCF,	"dal",  	# ISOarbc: ARABIC LETTER DAL
    0xD0,	"thal", 	# ISOarbc: ARABIC LETTER THAL
    0xD1,	"reh",  	# ISOarbc: ARABIC LETTER REH
    0xD2,	"zain", 	# ISOarbc: ARABIC LETTER ZAIN
    0xD3,	"seen", 	# ISOarbc: ARABIC LETTER SEEN
    0xD4,	"sheen",	# ISOarbc: ARABIC LETTER SHEEN
    0xD5,	"sad",  	# ISOarbc: ARABIC LETTER SAD
    0xD6,	"dad",  	# ISOarbc: ARABIC LETTER DAD
    0xD7,	"tah",  	# ISOarbc: ARABIC LETTER TAH
    0xD8,	"zah",  	# ISOarbc: ARABIC LETTER ZAH
    0xD9,	"ain",  	# ISOarbc: ARABIC LETTER AIN
    0xDA,	"ghain",	# ISOarbc: ARABIC LETTER GHAIN
    0xE0,	"tatweel",	# ISOarbc: ARABIC TATWEEL
    0xE1,	"feh",  	# ISOarbc: ARABIC LETTER FEH
    0xE2,	"qaf",  	# ISOarbc: ARABIC LETTER QAF
    0xE3,	"kaf",  	# ISOarbc: ARABIC LETTER KAF
    0xE4,	"lam",  	# ISOarbc: ARABIC LETTER LAM
    0xE5,	"meem", 	# ISOarbc: ARABIC LETTER MEEM
    0xE6,	"noon", 	# ISOarbc: ARABIC LETTER NOON
    0xE7,	"heh",  	# ISOarbc: ARABIC LETTER HEH
    0xE8,	"waw",  	# ISOarbc: ARABIC LETTER WAW
    0xE9,	"alefmaks",	# ISOarbc: ARABIC LETTER ALEF MAKSURA
    0xEA,	"yeh",  	# ISOarbc: ARABIC LETTER YEH
    0xEB,	"fathatan",	# ISOarbc: ARABIC FATHATAN
    0xEC,	"dammatan",	# ISOarbc: ARABIC DAMMATAN
    0xED,	"kasratan",	# ISOarbc: ARABIC KASRATAN
    0xEE,	"fatha",	# ISOarbc: ARABIC FATHA
    0xEF,	"damma",	# ISOarbc: ARABIC DAMMA
    0xF0,	"kasra",	# ISOarbc: ARABIC KASRA
    0xF1,	"shadda",	# ISOarbc: ARABIC SHADDA
    0xF2,	"sukun",	# ISOarbc: ARABIC SUKUN
);

%Ent2Char = reverse %Char2Ent;

1;
