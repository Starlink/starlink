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
