/*
*+
*  Name:
*     EMS1GNAM

*  Purpose:
*     Get the next name in a string.

*  Language:
*     Starlink ANSI C

*  Invocation:
*     ems1Gnam( string, iposn, name, namlen, status )

*  Description:
*     The given string is searched from index IPOSN+1 for the next name.
*     A name is defined as a contiguous string of alphanumeric and
*     underscore characters. The end of the string is given by the next
*     character which is not alphanumeric or an underscore (normally white
*     space). if the name string overflows the declared length of the
*     returned name argument, NAME, { the name string is returned
*     truncated to the length of the NAME argument and the status
*     argument is returned set. The character pointer IPOSN is returned
*     pointing to the last character of the name in the given string.

*  Arguments:
*     string = char* (Given)
*        The string to be searched for a name.
*     iposn = int* (Given and Returned)
*        Given as the pointer to the immediately before the first
*        element to be used in the name string search. Returned as the
*        last element of the name string.
*     name = char* (Returned)
*        The returned name string.
*     namlen = int* (Returned)
*        The length of the returned name string.
*     status = int* (Returned)
*        The global status.

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
*     AJC: A.J. Chipperfield (STARLINK)
*     {enter_new_authors_here}

*  History:
*     2-AUG-1991 (PCTR):
*        Original FORTRAN version.
*     14-FEB-2001 (RTP)
*        Rewritten in C based on the Fortran routine EMS1_GNAM
*     16-MAY-2001 (AJC):
*        Correctly terminate name string
*     24-OCT-2001 (AJC)
*        Allow _ in token name
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

#include <ctype.h>
#include "sae_par.h"          /* Standard SAE constants */
#include "ems_par.h"
#include "ems_sys.h"
#include "ems1.h"
#include "ems.h"

void ems1Gnam( const char *string, int *iposn,
                char *name, int *namlen, int *status ) {

   short begin;              /* Whether beginning the search */

   int ichr;                 /* Loop index */
   int mxlen;                /* Declared length of NAME */
   int strln;            /* Declared length of STRING */

   char cvalue;              /* Character value */

   TRACE("ems1Gnam");

/*  Initialise STATUS. */
   *status = SAI__OK;

/*  Initialize NAMLEN and STRLN. */
   *namlen = 0;
   strln = strlen( string + *iposn + 1 );

/*  Check that IPOSN points somewhere within the given string. */
   if ( strln > 0 ) {

/*     Calculate the total length of the string */
      strln += *iposn;

/*     Get the declared length of the name string. */
      mxlen = EMS__SZNAM;

/*     Initialize BEGIN. */
      begin = TRUE;

/*     Loop to get the name string. */
      for ( ichr = *iposn+1; ichr <= strln; ichr++ ) {

/*        Get the next character from the given string. */
         cvalue = string[ ichr ];

/*        Check for an end of name. */
         if ( begin ) {

/*           The first character of a name must be alphabetic. */
            if ( isalpha( cvalue ) ) {
               begin = FALSE;
            } else {
               break;
            }

/*        All remaining characters must be alphanumeric. */
         } else if ( !isalnum( cvalue ) &&
                     cvalue != '_' ) {
            break;
         }

/*        Increment the length of the name string and add the next
*        letter to the name string. */
         if ( *namlen == mxlen ) {
            *status = SAI__ERROR;
            break;
         } else {
            name[ (*namlen)++ ] = cvalue;
         }
      }
/*     Update IPOSN. */
      if ( ! begin && *status == SAI__OK ) *iposn = ichr-1;
   }

/* Terminate name string */
   name[*namlen] = '\0';

   return;
}
