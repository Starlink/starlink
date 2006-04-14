/*+
 *  Name:
 *     ems1Serr

 *  Purpose:
 *     Get the error message associated with a system error code.
 *     This is the Linux version.

 *  Language:
 *     Starlink ANSI C

 *  Invocation:
 *     ems1Serr( errstr, errlen, ernum_p)

 *  Description:
 *     This C function uses the system function strerror

 *  Arguments:
 *     errstr = char* (Returned)
 *        The System error message string.
 *     errlen = int (Given)
 *        The maximum length of errstr.
 *     errnum = int* (Given)
 *        A pointer to the error number.

*  Copyright:
*     Copyright (C) 1990, 1994 Science & Engineering Research Council.
*     Copyright (C) 2001, 2003 Central Laboratory of the Research Councils.
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
 *     PCTR: P.C.T. Rees (STARLINK)
 *     BKM: B.K.McIlwrath (STARLINK)
 *     RTP: R.T.Platon (STARLINK)
 *     AJC: A.J.Chipperfield (STARLINK)
 *     {enter_new_authors_here}

 *  History:
 *     23-MAY-1990 (PCTR):
 *        Original version.
 *     12-AUG-1994 (BKM):
 *        Change name from EMS1_GERR to EMS1_SERR
 *     20-FEb-2001 (RTP):
 *        Change name from EMS1_SERR to ems1Serr
 *     30-APR-2003 (AJC):
 *        Use strerror not sys_errlist
 *     {enter_further_changes_here}

 *  Bugs:
 *     {note_any_bugs_here}

 *- 
 */

#include <string.h>
#include "ems1.h"      /* EMS1_ internal functions */

/*  Function Definition: */
void ems1Serr( char *errstr, int errlen, int *errnum ) {

/*  Local Variables: */
   char *strerr;                  /* Pointer for system error message string */

/*    Load string. */
   strerr = ( *errnum>=0 )? strerror( *errnum ) : NULL;
   if( strerr != NULL ) {
      (void)strncpy( errstr, strerr, errlen );
   } else {
      *errstr = '\0';
   }
   return;
}
