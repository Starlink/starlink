/*
*+
*  Name:
*     ems1Putc

*  Purpose:
*     Put a CHARACTER string into another at a given position.

*  Language:
*     Starlink ANSI C

*  Invocation:
*     ems1Putc( cvalue, maxlen, string, iposn, status )

*  Description:
*     The string CVALUE (or as much of it as there is room for) is
*     copied into the part of STRING beginning at position IPOSN+1.
*     IPOSN is updated to indicate the end position of the copy of
*     CVALUE within STRING after this operation. If the resulting
*     string is truncated because STRING is too short, then the string
*     is terminated with an ellipsis and STATUS is returned set to 
*     EMS__STROV. The sizes of CVALUE and STRING are based on the 
*     declared Fortran 77 size given by the intrinsic function LEN. 

*  Implementation Notes:
*     The coding of this routine assumes that the need to append an 
*     ellipsis is rare: i.e. it is less efficient when it has to
*     append an ellipsis.

*  Arguments:
*     cvalue = char* (Given)
*        The string to be copied.
*     maxlen = int (Given)
*        Maximum length of output string
*     string = char* (Given and Returned)
*        The string into which CVALUE is to be copied.
*     iposn = int* (Given and Returned)
*        The position pointer within STRING.
*     status = int* (Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1983 Science & Engineering Research Council.
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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     JRG: Jack Giddings (UCL)
*     ACD: A.C. Davenhall (ROE)
*     AJC: A.J. Chipperfield (STARLINK)
*     PCTR: P.C.T. Rees (STARLINK)
*     RTP: R.T. Platon (STARLINK)
*     PWD: Peter W. Draper (JAC, Durham University)
*     {enter_new_authors_here}

*  History:
*     3-JAN-1983 (JRG):
*        Original FORTRAN version.
*     14-FEB-2001 (RTP)
*        Rewritten in C based on the Fortran routine EMS1_PUTC
*      5-MAR-2001 (AJC):
*        Add maxlen argument
*        Use *iposn not iposn internally 
*     21-SEP-2001 (AJC):
*        Correct calculation of allow
*     24-APR-2006 (PWD):
*        Correct strncpy to use allow, not allow + 1. Reserve 4 characters
*        for ellipsis ('...', plus '\0', strcpy includes trailing NULL ).
*        Calculate length of string to include trailing NULL (should
*        be copied if any later strlens are to succeed).
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

#include "sae_par.h"                 /* SAE public constants */
#include "ems_sys.h"                 /* SAE public constants */
#include "ems_par.h"                 /* EMS_ public constants */
#include "ems1.h"                    /* EMS_ internal functions */

void ems1Putc( const char *cvalue, const int maxlen, char *string, int *iposn,
               int *status ) {
   int allow;              /* Allowed length of CVALUE for copying */
   int idx;                /* Character index */
   int size1;              /* Used length of CVALUE */

   TRACE("ems1Putc");

   /*  Initialise the returned status. */
   *status = SAI__OK;

/*  Get the size of target string, including trailing NULL. */
   size1 = strlen( cvalue ) + 1;

/*  Check that the pointer is within string. */
   if ( *iposn < maxlen ) {

/*     Get the length that can be copied. */
      allow = MIN( size1, maxlen - *iposn - 1 );

/*     Copy the string. */
      strncpy( &string[*iposn+1] , cvalue, allow );

/*     Check if an ellipsis is required. */
      if ( allow < size1 ) {

/*        Append an ellipsis. */
         idx = MAX( 0, maxlen - 4 );
         strcpy( &string[idx], "..." );
         *status = SAI__WARN;
      }

/*     Update the pointer value. */
         *iposn = *iposn + allow - 1;

   } else {
/*     The pointer is beyond the declared length of the string, so 
 *     append an ellipsis. */
      idx = MAX( 0, maxlen - 4 );
      strcpy( &string[idx], "..." );
      *status = SAI__WARN;

/*     Update the pointer value. */
      *iposn = maxlen;
   }

   return;
}
