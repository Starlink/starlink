/*+
 *  Name:
 *     emsErrno

 *  Purpose:
 *     Assign the message associated with an errno value to a token (Unix)

 *  Language:
 *     Starlink ANSI C

 *  Invocation:
 *     emsErrno( token, errval )

 *  Description:
 *     On Unix errno values and system error codes are identical so this
 *     function just uses EMS1_SERR to obtain the message and outputs
 *     an appropriate message if no message is returned.

 *  Arguments:
 *     token = const char * (Given)
 *        The message token to be associated with the error text.
 *     errval = int (Given)
 *        The errno value.

*  Copyright:
*     Copyright (C) 1994 Science & Engineering Research Council.
*     Copyright (C) 1999, 2001 Central Laboratory of the Research Councils.
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
 *     BKM: B.K. McIlwrath (STARLINK)
 *     AJC: A.J.Chipperfield (STARLINK)
 *     RTP: R.T.Platon (STARLINK)
 *     {enter_new_authors_here}

 *  History:
 *     12-AUG-1994 (BKM):
 *        Original version.
 *     14-SEP-1994 (AJC):
 *        Use EMS1_SERR to allow more appropriate default message
 *     13-MAY-1999 (AJC):
 *        Renamed from ems_errno_c
 *        and use emsSetx not ems_setx_c
 *     14-FEB-2001 (RTP):
 *        Rewritten in C from Fortran routine EMSERRNO
 *      2-AUG-2001 (AJC):
 *        removed Fortran interface
 *      8-AUG-2001 (AJC):
 *        Change ems1_serr to ems1Serr
 *      15-SEP-2008 (TIMJ):
 *        3-arg emsSetc is deprecated.
 *     {enter_further_changes_here}

 *  Bugs:
 *     {note_any_bugs_here}

 *-
 */

/* Include Statements: */
#include <string.h>                    /* String handling library functions */
#include "ems_par.h"                   /* ems_ public constant definitions */
#include "ems_sys.h"                   /* ems_ private macro definitions */
#include "ems.h"                       /* ems_ function prototypes */
#include "ems1.h"                      /* ems_ internal function prototypes */

/* Function Definitons: */
void emsErrno( const char *token, int errval ){

   char mess[EMS__SZTOK+1];  /* Message associated with errval */

   TRACE("emsErrno");

/* Call EMS1_SERR */
   ems1Serr( mess, EMS__SZTOK, &errval );

   mess[EMS__SZTOK] = '\0';

/* Check for a good translation */
   if ( strspn(mess," ") != EMS__SZTOK ){
/*   OK - put the mesage in a token */
      emsSetc( token, mess);
   } else {
/*   Bad - construct an error message */
      emsSetc( token, "No translation for errno");
      emsSetc( token, " ");
      emsSeti( token, errval );
   }
   return;
}
