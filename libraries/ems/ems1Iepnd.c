/*
*+
*  Name:
*     EMS1IEPND

*  Purpose:
*     Return whether there are any error messages pending output.

*  Language:
*     Starlink ANSI C

*  Invocation:
*     result = ems1Iepnd( )

*  Description:
*     The number of error messages at the current context in the error 
*     table is found. If there are any entries pending output then
*     EMS1IEPND returns the value TRUE, otherwise the value FALSE 
*     is returned.

*  Authors:
*     PCTR: P.C.T. Rees (STARLINK)
*     RTP: R.T. Platon (STARLINK)
*     {enter_new_authors_here}

*  History:
*     8-JAN-90 (PCTR):
*        Original FORTRAN version.
*     14-FEB-2001 (RTP)
*        Rewritten in C based on the Fortran routine EMS1_IEPND
*     {enter_any_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

#include "ems1.h"                    /* EMS_ Internal functions */
#include "ems_par.h"                 /* EMS_ public constants */
#include "ems_sys.h"                 /* EMS_ private constants */

#include "ems_msgtb.h"               /* Error message table */

Logical ems1Iepnd ( void ) {
   int n;

   TRACE("ems1Iepnd");

/*  Find the number of pending error messages at the current error context. */
   if ( msgmrk > EMS__BASE ) {
      n = msgcnt[ msgmrk ] - msgcnt[ msgmrk - 1 ];
   } else {
      n = msgcnt[ msgmrk ];
   }

/*  Set EMS1_IEPND. */
   return ( n > 0 );

}
