##---------------------------------------------------------------------------##
##  File:
##      @(#) S5.pm 1.1 97/09/11 17:41:12 @(#)
##  Author:
##      Earl Hood       ehood@medusa.acs.uci.edu
##  Description:
##      Mappings for ISO-8859-5.
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

package SGML::ISO8859::S5;

##---------------------------------------------------------------------------
##      ISO-8859-5: Cyrillic
##---------------------------------------------------------------------------

%Char2Ent = (
  #--------------------------------------------------------------------------
  # Hex Code	Entity Ref	# ISO external entity and description
  #--------------------------------------------------------------------------
    0xA1,	"IOcy", 	# ISOcyr1: CYRILLIC CAPITAL LETTER IO
    0xA2,	"DJcy", 	# ISOcyr2: CYRILLIC CAPITAL LETTER DJE
				#	   (Serbocroatian)
    0xA3,	"GJcy", 	# ISOcyr2: CYRILLIC CAPITAL LETTER GJE
				#	   (Macedonian)
    0xA4,	"IEcy", 	# ISOcyr1: CYRILLIC CAPITAL LETTER UKRAINIAN IE
    0xA5,	"DScy", 	# ISOcyr2: CYRILLIC CAPITAL LETTER DZE
				#	   (Macedonian)
    0xA6,	"Iukcy",	# ISOcyr2: CYRILLIC CAPITAL LETTER
				#	   BYELORUSSIAN-UKRAINIAN I
    0xA7,	"YIcy", 	# ISOcyr2: CYRILLIC CAPITAL LETTER YI
				#	   (Ukrainian)
    0xA8,	"Jukcy",	# ISOcyr2: CYRILLIC CAPITAL LETTER JE
    0xA9,	"LJcy", 	# ISOcyr2: CYRILLIC CAPITAL LETTER LJE
    0xAA,	"NJcy", 	# ISOcyr2: CYRILLIC CAPITAL LETTER NJE
    0xAB,	"TSHcy",	# ISOcyr2: CYRILLIC CAPITAL LETTER TSHE
				#	   (Serbocroatian)
    0xAC,	"KJcy", 	# ISOcyr2: CYRILLIC CAPITAL LETTER KJE
				#	   (Macedonian)
    0xAD,	"shy",  	# ISOnum : SOFT HYPHEN
    0xAE,	"Ubrcy",	# ISOcyr2: CYRILLIC CAPITAL LETTER SHORT U
				#	   (Byelorussian)
    0xAF,	"DZcy", 	# ISOcyr2: CYRILLIC CAPITAL LETTER DZHE
    0xB0,	"Acy",  	# ISOcyr1: CYRILLIC CAPITAL LETTER A
    0xB1,	"Bcy",  	# ISOcyr1: CYRILLIC CAPITAL LETTER BE
    0xB2,	"Vcy",  	# ISOcyr1: CYRILLIC CAPITAL LETTER VE
    0xB3,	"Gcy",  	# ISOcyr1: CYRILLIC CAPITAL LETTER GHE
    0xB4,	"Dcy",  	# ISOcyr1: CYRILLIC CAPITAL LETTER DE
    0xB5,	"IEcy", 	# ISOcyr1: CYRILLIC CAPITAL LETTER IE
    0xB6,	"ZHcy", 	# ISOcyr1: CYRILLIC CAPITAL LETTER ZHE
    0xB7,	"Zcy",  	# ISOcyr1: CYRILLIC CAPITAL LETTER ZE
    0xB8,	"Icy",  	# ISOcyr1: CYRILLIC CAPITAL LETTER I
    0xB9,	"Jcy",  	# ISOcyr1: CYRILLIC CAPITAL LETTER SHORT I
    0xBA,	"Kcy",  	# ISOcyr1: CYRILLIC CAPITAL LETTER KA
    0xBB,	"Lcy",  	# ISOcyr1: CYRILLIC CAPITAL LETTER EL
    0xBC,	"Mcy",  	# ISOcyr1: CYRILLIC CAPITAL LETTER EM
    0xBD,	"Ncy",  	# ISOcyr1: CYRILLIC CAPITAL LETTER EN
    0xBE,	"Ocy",  	# ISOcyr1: CYRILLIC CAPITAL LETTER O
    0xBF,	"Pcy",  	# ISOcyr1: CYRILLIC CAPITAL LETTER PE
    0xC0,	"Rcy",  	# ISOcyr1: CYRILLIC CAPITAL LETTER ER
    0xC1,	"Scy",  	# ISOcyr1: CYRILLIC CAPITAL LETTER ES
    0xC2,	"Tcy",  	# ISOcyr1: CYRILLIC CAPITAL LETTER TE
    0xC3,	"Ucy",  	# ISOcyr1: CYRILLIC CAPITAL LETTER U
    0xC4,	"Fcy",  	# ISOcyr1: CYRILLIC CAPITAL LETTER EF
    0xC5,	"KHcy", 	# ISOcyr1: CYRILLIC CAPITAL LETTER HA
    0xC6,	"TScy", 	# ISOcyr1: CYRILLIC CAPITAL LETTER TSE
    0xC7,	"CHcy", 	# ISOcyr1: CYRILLIC CAPITAL LETTER CHE
    0xC8,	"SHcy", 	# ISOcyr1: CYRILLIC CAPITAL LETTER SHA
    0xC9,	"SHCHcy",	# ISOcyr1: CYRILLIC CAPITAL LETTER SHCHA
    0xCA,	"HARDcy",	# ISOcyr1: CYRILLIC CAPITAL LETTER HARD SIGN
    0xCB,	"Ycy",  	# ISOcyr1: CYRILLIC CAPITAL LETTER YERU
    0xCC,	"SOFTcy",	# ISOcyr1: CYRILLIC CAPITAL LETTER SOFT SIGN
    0xCD,	"Ecy",  	# ISOcyr1: CYRILLIC CAPITAL LETTER E
    0xCE,	"YUcy", 	# ISOcyr1: CYRILLIC CAPITAL LETTER YU
    0xCF,	"YAcy", 	# ISOcyr1: CYRILLIC CAPITAL LETTER YA
    0xD0,	"acy",  	# ISOcyr1: CYRILLIC SMALL LETTER A
    0xD1,	"bcy",  	# ISOcyr1: CYRILLIC SMALL LETTER BE
    0xD2,	"vcy",  	# ISOcyr1: CYRILLIC SMALL LETTER VE
    0xD3,	"gcy",  	# ISOcyr1: CYRILLIC SMALL LETTER GHE
    0xD4,	"dcy",  	# ISOcyr1: CYRILLIC SMALL LETTER DE
    0xD5,	"iecy", 	# ISOcyr1: CYRILLIC SMALL LETTER IE
    0xD6,	"zhcy", 	# ISOcyr1: CYRILLIC SMALL LETTER ZHE
    0xD7,	"zcy",  	# ISOcyr1: CYRILLIC SMALL LETTER ZE
    0xD8,	"icy",  	# ISOcyr1: CYRILLIC SMALL LETTER I
    0xD9,	"jcy",  	# ISOcyr1: CYRILLIC SMALL LETTER SHORT I
    0xDA,	"kcy",  	# ISOcyr1: CYRILLIC SMALL LETTER KA
    0xDB,	"lcy",  	# ISOcyr1: CYRILLIC SMALL LETTER EL
    0xDC,	"mcy",  	# ISOcyr1: CYRILLIC SMALL LETTER EM
    0xDD,	"ncy",  	# ISOcyr1: CYRILLIC SMALL LETTER EN
    0xDE,	"ocy",  	# ISOcyr1: CYRILLIC SMALL LETTER O
    0xDF,	"pcy",  	# ISOcyr1: CYRILLIC SMALL LETTER PE
    0xE0,	"rcy",  	# ISOcyr1: CYRILLIC SMALL LETTER ER
    0xE1,	"scy",  	# ISOcyr1: CYRILLIC SMALL LETTER ES
    0xE2,	"tcy",  	# ISOcyr1: CYRILLIC SMALL LETTER TE
    0xE3,	"ucy",  	# ISOcyr1: CYRILLIC SMALL LETTER U
    0xE4,	"fcy",  	# ISOcyr1: CYRILLIC SMALL LETTER EF
    0xE5,	"khcy", 	# ISOcyr1: CYRILLIC SMALL LETTER HA
    0xE6,	"tscy", 	# ISOcyr1: CYRILLIC SMALL LETTER TSE
    0xE7,	"chcy", 	# ISOcyr1: CYRILLIC SMALL LETTER CHE
    0xE8,	"shcy", 	# ISOcyr1: CYRILLIC SMALL LETTER SHA
    0xE9,	"shchcy",	# ISOcyr1: CYRILLIC SMALL LETTER SHCHA
    0xEA,	"hardcy",	# ISOcyr1: CYRILLIC SMALL LETTER HARD SIGN
    0xEB,	"ycy",  	# ISOcyr1: CYRILLIC SMALL LETTER YERU
    0xEC,	"softcy",	# ISOcyr1: CYRILLIC SMALL LETTER SOFT SIGN
    0xED,	"ecy",  	# ISOcyr1: CYRILLIC SMALL LETTER E
    0xEE,	"yucy", 	# ISOcyr1: CYRILLIC SMALL LETTER YU
    0xEF,	"yacy", 	# ISOcyr1: CYRILLIC SMALL LETTER YA
    0xF0,	"numero",	# ISOcyr1: NUMERO SIGN
    0xF1,	"iocy", 	# ISOcyr1: CYRILLIC SMALL LETTER IO
    0xF2,	"djcy", 	# ISOcyr2: CYRILLIC SMALL LETTER DJE
				#	   (Serbocroatian)
    0xF3,	"gjcy", 	# ISOcyr2: CYRILLIC SMALL LETTER GJE
				#	   (Macedonian)
    0xF4,	"iecy", 	# ISOcyr1: CYRILLIC SMALL LETTER UKRAINIAN IE
    0xF5,	"dscy", 	# ISOcyr2: CYRILLIC SMALL LETTER DZE
				#	   (Macedonian)
    0xF6,	"iukcy",	# ISOcyr2: CYRILLIC SMALL LETTER
				#	   BYELORUSSIAN-UKRAINIAN I
    0xF7,	"yicy", 	# ISOcyr2: CYRILLIC SMALL LETTER YI
				#	   (Ukrainian)
    0xF8,	"jsercy",	# ISOcyr2: CYRILLIC SMALL LETTER JE
    0xF9,	"ljcy", 	# ISOcyr2: CYRILLIC SMALL LETTER LJE
    0xFA,	"njcy", 	# ISOcyr2: CYRILLIC SMALL LETTER NJE
    0xFB,	"tshcy",	# ISOcyr2: CYRILLIC SMALL LETTER TSHE
				#	   (Serbocroatian)
    0xFC,	"kjcy", 	# ISOcyr2: CYRILLIC SMALL LETTER KJE
				#	   (Macedonian)
    0xFD,	"sect", 	# ISOnum : SECTION SIGN
    0xFE,	"ubrcy",	# ISOcyr2: CYRILLIC SMALL LETTER SHORT U
				#	   (Byelorussian)
    0xFF,	"dzcy", 	# ISOcyr2: CYRILLIC SMALL LETTER DZHE
);

%Ent2Char = reverse %Char2Ent;

1;
