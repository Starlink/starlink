/*
*+
*  Name:
*     EMS1ERLSE

*  Purpose:
*     Release a context in the error table.

*  Language:
*     Starlink ANSI C

*  Invocation:
*     ems1Erlse()

*  Description:
*     If the error context level has been set, then the previous context
*     is returned to. Any messages remaining at the current context 
*     level will be transferred to the previous level.

*  Copyright:
*     Copyright (C) 1983 Science & Engineering Research Council.
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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     SLW: Sid Wright (UCL)
*     RFWS: R.F. Warren-Smith (STARLINK)
*     PCTR: P.C.T. Rees (STARLINK)
*     RTP: R.T. Platon (STARLINK)
*     {enter_new_authors_here}

*  History:
*     17-Apr-1983 (SLW):
*        Original FORTRAN version.
*     14-FEB-2001 (RTP):
*        Rewritten in C based on the Fortran routine EMS1_ERLSE
*     19-MAR-2001 (AJC):
*        Correct 'msglev < EMS__MXLEV' to <=
*     13-AUG-2001 (AJC):
*        #include ems1.h
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

#include "ems1.h"                    /* EMS1 function prototypes */
#include "ems_par.h"                 /* EMS_ public constants */
#include "ems_sys.h"                 /* EMS_ private constants */

/*  Global Variables: */
#include "ems_msgtb.h"               /* Error message table */

void ems1Erlse( void ) {
   int istat;                     /* Local status */
   int lstat;                     /* Local status */
   int newcnt;                    /* Updated MSGCNT( MSGMRK ) */
      
   TRACE("ems1Erlse");
   DEBUG("ems1Erlse","BEFORE msglev = %d", msglev );

/*  If the context level is marked above EMS__MXLEV, lower the level mark
 *  only. If the context level within the allowed range: if there is 
 *  more than one context marker, remove last one, transferring any
 *  messages associated with it to the next context down.
 */
   if ( msglev > EMS__MXLEV ) {

/*     Context stack overflow. */
      msglev--;

   } else if ( msglev <= EMS__MXLEV && msglev > msgdef ) {

/*     All marked context levels. */
      newcnt = msgcnt[ msgmrk ];
      msglev--;
      msgmrk--;
      msgcnt[ msgmrk ] = newcnt;

/*     If the new error context is the base level, flush and pending error
 *     messages and update the last reported status (MSGLST).
 */
      if ( msglev == EMS__BASE && newcnt > 0 ) {
         lstat = msgsta[ newcnt ];
         istat = lstat;
         ems1Flush( &istat );
         msglst = lstat;
      }
   } else {

/*     Otherwise, do nothing. */
      msglev = msgdef;
      msgmrk = msgdef;
   }
   DEBUG("ems1Erlse","AFTER msglev = %d", msglev );

   return;
}
