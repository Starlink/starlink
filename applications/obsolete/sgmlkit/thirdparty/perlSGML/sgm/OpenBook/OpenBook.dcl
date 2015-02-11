<!SGML "ISO 8879:1986"

   -- SGML Declaration

     (c) Copyright 1987-1993 Hewlett-Packard Company
    
     Permission to use, copy, and distribute this Document Type
     Definition (DTD) is hereby granted, provided that the above
     copyright notice appear in all copies and that both that copyright
     notice and this permission notice appear in supporting hardcopy and
     online documentation.  All other rights reserved.
    
     The name of Hewlett-Packard Company or the Hewlett-Packard logo may
     not be used in advertising or publicity pertaining to distribution
     of this DTD without specific, written prior permission.
     Hewlett-Packard Company makes no representations about the
     suitability of this DTD for any purpose.  It is provided "as is"
     without express or implied warranty.
    
     Hewlett-Packard disclaims all warranties with regard to this DTD,
     including all implied warranties of merchantability and fitness, in
     no event shall Hewlett-Packard Company be liable for any special,
     indirect or consequential damages or any damages whatsoever
     resulting from loss of use, data or profits, whether in an action
     of contract, negligence or other tortious action, arising out of or
     in connection with the use or performance of this DTD.

     Revision Control System (RCS) Information:

          $Header: openbook.dtd,v 2.1.1.2 94/01/18 11:45:05 dmh Rel $

   --



-- ___________________________ SGML DECLARATION _____________________ --

CHARSET
BASESET "ISO  646-1983//CHARSET International Reference  Version
         (IRV)//ESC 2/5 4/0"
DESCSET     0    9   UNUSED
     9    2   9
    11    2   UNUSED
    13    1   13
    14   18   UNUSED
    32   95   32
   127    1   UNUSED
BASESET   "ISO Registration Number 100//CHARSET ECMA-94
           Right Part of  Latin Alphabet  Nr. 1//ESC  2/13 4/1"
DESCSET   128         32   128 	-- changed from UNUSED on 1/18/94 --
   160          5   32
   165          1   165 	-- changed from UNUSED on 1/18/94 --
   166         88   166		-- changed from 38 on 1/18/94 --
   254          1   254 	-- changed from 127 on 1/18/94 --
   255          1   255 	-- changed from UNUSED on 1/18/94 --


   CAPACITY SGMLREF 
      TOTALCAP 350000  
      ENTCAP   100000 
      ENTCHCAP 50000
      ELEMCAP  50000
      GRPCAP   210000
      EXGRPCAP 50000
      EXNMCAP  50000
      ATTCAP   50000
      ATTCHCAP 50000
      AVGRPCAP 50000
      NOTCAP   50000
      NOTCHCAP 50000
      IDCAP    50000
      IDREFCAP 50000
      MAPCAP   210000
      LKSETCAP 50000
      LKNMCAP  50000
   SCOPE DOCUMENT
   SYNTAX -- The Core Reference Syntax except with ATTCNT, LITLEN
             NAMELEN, GRPCNT and GRPGTCNT changed --

SHUNCHAR CONTROLS 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17
   18 19 20 21 22 23 24 25 26 27 28 29 30 31 127 
	-- removed 255 on 1/18/94 --
BASESET "ISO  646-1983//CHARSET International Reference  Version
         (IRV)//ESC 2/5 4/0"
DESCSET    0          128          0
FUNCTION   RE          13
           RS          10
           SPACE       32
           TAB        SEPCHAR      9

      NAMING
         LCNMSTRT ""
         UCNMSTRT ""
         LCNMCHAR "-."
         UCNMCHAR "-."
         NAMECASE
            GENERAL YES
            ENTITY  NO
      DELIM
         GENERAL SGMLREF
--  Removed short references --
         SHORTREF SGMLREF 
      NAMES SGMLREF
      QUANTITY SGMLREF
         ATTCNT 140
         LITLEN 2048
         NAMELEN 64
         GRPCNT 100   
         GRPGTCNT 300 
         TAGLVL 48    -- Added line 2/7/92 to prevent BadFont abort caused by
                         having more than 26 eics for a given tag --
         FEATURES -- only OMITTAG and FORMAL --
   MINIMIZE DATATAG NO OMITTAG YES RANK NO SHORTTAG NO
   LINK SIMPLE NO IMPLICIT NO EXPLICIT NO
   OTHER CONCUR NO SUBDOC NO FORMAL YES
   APPINFO NONE
>
