#include "f77.h"
      F77_SUBROUTINE(chr_etom)( CHARACTER(str1), CHARACTER(str2)
                                TRAIL(str1) TRAIL(str2) ) {
/*
*+
*  Name:
*     CHR_ETOM

*  Purpose:
*     Translate a string from EBCDIC to the machine's character set.

*  Language:
*     Fortran callable C

*  Invocation:
*     CALL CHR_ETOM( STR1, STR2 )

*  Description:
*     The string STR1, which has been written on a machine which uses
*     the EBCDIC character set and subsequently read on a machine which
*     may not use the EBCDIC character set to represent characters in
*     Fortran, is returned in STR2 translated into the correct
*     character set for the host machine.

*  Arguments:
*     STR1 = CHARACTER * ( * ) (Given)
*        The character string written on a machine with an EBCDIC
*        character set and read on a machine which may not use
*        EBCDIC to represent characters in Fortran.
*     STR2 = CHARACTER * ( * ) (Returned)
*        The translated EBCDIC character string. If STR2 is shorter
*        than STR1, the translated string will be truncated; if
*        STR2 is longer than STR1, STR2 will be padded with blanks
*        beyond the translated string.

*  Note:
*     This subroutine has been implemented for machines which use
*     the ASCII character set.

*  Copyright:
*     Copyright (C) 1991, 1994 Science & Engineering Research Council.
*     Copyright (C) 2001 Central Laboratory of the Research Councls.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     PCTR: P.C.T. Rees (STARLINK)
*     ACC:  A.C. Charles (STARLINK)
*     AJC: A.J. Chipperfield (STARLINK)
*     {enter_new_authors_here}

*  History:
*     25-FEB-1991 (PCTR):
*        Original version.
*     10-MAR-1994 (ACC):
*        Modifications to prologue.
*      3-DEC-2001 (AJC):
*        Re-write in C for better portability
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

/*  Local Constants: */
#define EBCMAX 255 /* Maximum EBCDIC character code */

/*  Local Variables: */
int ichr; /* Character loop index */
int iebc; /* EBCDIC character code */
int length;
int table[EBCMAX+1]= {
   0,   1,   2,   3,  32,   9,  32, 127,  32,  32,
  32,  11,  12,  13,  14,  15,  16,  17,  18,  19,
  32,  32,   8,  32,  24,  25,  32,  32,  28,  29,
  30,  31,  32,  32,  32,  32,  32,  10,  23,  27,
  32,  32,  92,  32,  32,   5,   6,   7,  32,  32,
  22,  32,  32,  32,  32,   4,  32,  32,  32,  32,
  20,  21,  32,  26,  32,  32,  32,  32,  32,  32,
  32,  32,  32,  32,  91,  46,  60,  40,  43,  33,
  38,  32,  32,  32,  32,  32,  32,  32,  32,  32,
  93,  36,  42,  41,  59,  94,  45,  47,  32,  32,
  32,  32,  32,  32,  32,  32, 124,  44,  37,  95,
  62,  63,  32,  32,  32,  32,  32,  32,  32,  32,
  32,  96,  58,  35,  64,  39,  61,  34,  32,  97,
  98,  99, 100, 101, 102, 103, 104, 105,  32,  32,
  32,  32,  32,  32,  32, 106, 107, 108, 109, 110,
 111, 112, 113, 114,  32,  32,  32,  32,  32,  32,
  32, 126, 115, 116, 117, 118, 119, 120, 121, 122,
  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,
  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,
  32,  32, 123,  65,  66,  67,  68,  69,  70,  71,
  72,  73,  32,  32,  32,  32,  32,  32, 125,  74,
  75,  76,  77,  78,  79,  80,  81,  82,  32,  32,
  32,  32,  32,  32,  32,  32,  83,  84,  85,  86,
  87,  88,  89,  90,  32,  32,  32,  32,  32,  32,
  48,  49,  50,  51,  52,  53,  54,  55,  56,  57,
  32, 32, 32, 32, 32, 32,
};

/*  Get the maximum loop index.*/
      length = str1_length > str2_length ? str2_length: str1_length;

/*  If the string length is non-zero, loop to translate it from EBCDIC to */
/*  ASCII. */
      if ( length > 0 ) {

/*     If the machine value is legitimate, translate it; */
/*     if not, translate it to an machine code SPACE. */
         for (ichr=0; ichr<str2_length; ichr++ ) {
            if ( ichr > length ) {
               str2[ichr] = ' ';
            } else {
               iebc = (int) (unsigned char) str1[ichr];
               if ( iebc < EBCMAX ) {
                  str2[ichr] = (char)table[iebc];
               } else {
                  str2[ichr] = ' ';
               }
            }
         }
      }
}
