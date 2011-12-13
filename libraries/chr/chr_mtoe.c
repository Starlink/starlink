#include "f77.h"
      F77_SUBROUTINE(chr_mtoe)( CHARACTER(str1), CHARACTER(str2)
                                TRAIL(str1) TRAIL(str2) ) {
/*
*+
*  Name:
*     CHR_MTOE

*  Purpose:
*     Translate a string from the machine's character set to EBCDIC.

*  Language:
*     Fortran callable C

*  Invocation:
*     CALL CHR_MTOE( STR1, STR2 )

*  Description:
*     The string STR1, which is a Fortran 77 CHARACTER string, is
*     returned in STR2 translated into a form which can be written and
*     subsequently read correctly by a machine which uses the EBCDIC
*     character set.

*     Any characters which are not represented in the
*     EBCDIC character set are translated to EBCDIC SPACE. Non-printable
*     characters are translated where possible.

*  Arguments:
*     STR1 = CHARACTER * ( * ) (Given)
*        The Fortran 77 character string.
*     STR2 = CHARACTER * ( * ) (Returned)
*        A character string which may be written and subsequently read
*        correctly by a machine which uses the EBCDIC character set
*        to represent characters in Fortran. If STR2 is shorter than
*        STR1, the translated string will be truncated; if STR2 is
*        longer than STR1, STR2 will be padded with blanks beyond the
*        translated string.

*  System-specific:
*     This subroutine has been implemented for machines which use
*     the ASCII character set.

*  Copyright:
*     Copyright (C) 1992, 1994 Science & Engineering Research Council.
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
*     {enter_new_authors_here}

*  History:
*     10-JUL-1992 (PCTR):
*        Original version.
*     10-MAR-1994 (ACC):
*        Modifications to prologue.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/
/*  Local Constants: */
#define MCHMAX 127 /* Maximum machine character code */
#define EBCSPC 64  /* EBCDIC SPACE */

/*  Local Variables: */
int ichr;               /* Character loop index */
int imch;               /* Machine character code */
int length;             /* Maximum loop index. */
int table[MCHMAX+1] = {
  0,   1,   2,   3,  55,  45,  46,  47,  22,   5,
 37,  11,  12,  13,  14,  15,  16,  17,  18,  19,
 60,  61,  50,  38,  24,  25,  63,  39,  28,  29,
 30,  31,  64,  79, 127, 123,  91, 108,  80, 125,
 77,  93,  92,  78, 107,  96,  75,  97, 240, 241,
242, 243, 244, 245, 246, 247, 248, 249, 122,  94,
 76, 126, 110, 111, 124, 193, 194, 195, 196, 197,
198, 199, 200, 201, 209, 210, 211, 212, 213, 214,
215, 216, 217, 226, 227, 228, 229, 230, 231, 232,
233,  74, 224,  90,  95, 109, 121, 129, 130, 131,
132, 133, 134, 135, 136, 137, 145, 146, 147, 148,
149, 150, 151, 152, 153, 162, 163, 164, 165, 166,
167, 168, 169, 192, 106, 208, 161,   7
};

/*  Get the maximum loop index. */
      length = str1_length > str2_length ? str2_length: str1_length;

/*  If the string length is non-zero, loop to translate it from ASCII to */
/*  EBCDIC. */
      if ( length > 0 ) {

/*     If the machine value is legitimate, translate it; */
/*     if not, translate it to an EBCDIC SPACE. */
         for (ichr=0; ichr<str2_length; ichr++ ) {
            if ( ichr > length ) {
               str2[ichr] = EBCSPC;
            } else {
               imch = (int) (unsigned char) str1[ichr];
               if ( imch < MCHMAX ) {
                  str2[ichr] = (char)table[imch];
               } else {
                  str2[ichr] = EBCSPC;
               }
            }
         }
      }
}
