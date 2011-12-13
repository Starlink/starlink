/*
*+
*  Name:
*     EMS1GESC

*  Purpose:
*     Get the next occurrence of a set of specified escape chars.

*  Language:
*     Starlink ANSI C

*  Invocation:
*     ems1Gesc( escchr, string, iposn )

*  Description:
*     The given string is searched forward from the index IPOSN+1 for the
*     next occurrence of any of the chars given in the escape string.
*     The char pointer IPOSN is returned pointing to this next escape
*     char in the given string. if no escape char is found,
*     IPOSN is returned set to zero.

*  Arguments:
*     escchr = char* (Given)
*        The set of given escape chars.
*     string = char* (Given)
*        The string to be searched.
*     iposn = int* (Given and Returned)
*        Given as the pointer to the previous escape char, returned
*        as the pointer to the next escape char. IPOSN is given as
*        0 to indicate the start of the search and is returned set to
*        the index of the next escape char: when no more escape
*        chars can be found in the given string, IPOSN is returned
*        as 0.

*  Copyright:
*     Copyright (C) 1991 Science & Engineering Research Council.
*     Copyright (C) 2001 Central Laboratory of the Research Councils.
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
*     RTP: R.T. Platon (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1-AUG-1991 (PCTR):
*        Original FORTRAN version.
*     14-FEB-2001 (RTP)
*        Rewritten in C based on the Fortran routine EMS1_PFORM
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

#include <string.h>
#include "ems_par.h"     /* EMS constants */
#include "ems_sys.h"     /* EMS internal functions */
#include "ems1.h"        /* EMS1_ function prototypes */
#include "ems.h"

void ems1Gesc( const char *escchr, const char *string, int *iposn ) {
   int esclen;             /* Length of the escape char string */
   int ichr;               /* Loop index */
   int idx;                /* Char index */
   int newpos;             /* Index of the next escape char */
   int strln;              /* Declared length of STRING */
   char *index;

   TRACE("ems1Gesc");

/*  Get the length of the given strings. */
   esclen = strlen( escchr );
   strln = strlen( string );

/*  Initialise NEWPOS. */
   newpos = -1;

/*  Check that IPOSN points to somewhere within the string and that
*  the length of the escape char string is non-zero. */
   if ( *iposn+1 < strln && esclen > 0 ) {

/*     Loop to get the index of the next escape char. */
      for ( ichr = 0; ichr < esclen; ichr++ ) {
         index = strchr( &string[*iposn+1], escchr[ ichr ] );
         if ( index != NULL ) {
            idx = index - string - *iposn;
            newpos = ( newpos == -1 ? idx : MIN( idx, newpos ) );
         }
      }
   }

/*  Update IPOSN. */
   if ( newpos >= 0 ) {
      *iposn += newpos;
   } else {
      *iposn = -1;
   }

   return;
}
