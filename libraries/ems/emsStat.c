/*+
 *  Name:
 *     emsStat

 *  Purpose:
 *     Return the last status set within the current error context.

 *  Language:
 *     Starlink ANSI C

 *  Invocation:
 *     emsStat( status )

 *  Description:
 *     This function provides the functionality of the Error Message 
 *     Service routine EMS_STAT (written in Fortran).

 *  Arguments:
 *     status = int * (Returned)
 *        The global status value.

*  Copyright:
*     Copyright (C) 1990 Science & Engineering Research Council.
*     Copyright (C) 1999, 2001 Central Laboratory of the Research Councls.
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
 *     AJC: A.J. Chipperfield (STARLINK)
 *     RTP: R.T.Platon (STARLINK)
 *     {enter_new_authors_here}

 *  History:
 *     8-NOV-1990 (PCTR):
 *        Original version.
 *     13-MAY-1999 (AJC):
 *        Renamed from ems_stat_c
 *     14-Feb-2001 (RTP):
 *        rewritten EMS_STAT in C
 *     {enter_further_changes_here}

 *  Bugs:
 *     {note_any_bugs_here}

 *-
 */

/* Include Statements: */
#include "sae_par.h"                   /* SAE_ public constant definitions */
#include "ems_par.h"                   /* EMS_ public constant definitions */
#include "ems_sys.h"                   /* EMS_ private macro definitions */
#include "ems_err.h"                   /* EMS_ error codes definitions */
#include "ems.h"                       /* EMS_ function prototypes */
#include "ems_msgtb.h"                 /* EMS_ message table */

/* Function Definitions: */
void emsStat( int *status ){
   int istart;
   int iend;

   TRACE("emsStat");

/*  Check for context stack overflow. */
   if ( msglev <= EMS__MXLEV ) {

/*     Find the first message in the current context. */
      if ( msgmrk == EMS__BASE ) {

/*        The base context is flushed immediately, the last reported 
 *        status value is stored in MSGLST.
 */
         *status = msglst;
      } else {

/*        Higher contexts use the error table. First find the position of 
 *        the first error message in the current context.
 */
        istart = msgcnt[ msgmrk - 1 ] + 1;

/*        Find the last message. */
        iend = msgcnt[ msgmrk ];

/*        If there are any messages in the current context set STATUS to 
 *        the last reported value, else return SAI__OK.
 */
        if ( iend >= istart ) {
           *status = msgsta[ iend ];
        } else {
           *status = SAI__OK;
        }
      }
   } else {

/*     The error context stack has overflowed, so return the appropriate
 *     error status.
 */
      *status = EMS__CXOVF;
   }

   return;
}
