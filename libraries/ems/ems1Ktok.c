/*
*+
*  Name:
*     EMS1KTOK

*  Purpose:
*     Clear the message token table.

*  Language:
*     Starlink ANSI C

*  Invokation:
*     ems1Ktok()

*  Description:
*     Clear all the message tokens at the current context level.

*  Algorithm:
*     -  Set the token count to that of previous context level.

*  Authors:
*     JRG: Jack Giddings (UCL)
*     SLW: Sid Wright (UCL)
*     PCTR: P.C.T. Rees (STARLINK)
*     RTP: R.T. Platon (STARLINK)
*     AJC: A.J.Chipperfield (STARLINK)
*     {enter_new_authors_here}

*  History:
*     3-JAN-1982 (JRG):
*        Original FORTRAN version.
*     14-FEB-2001 (RTP):
*        Rewritten in C based on the Fortran routine EMS1_PFORM
*      1-OCT-2001 (AJC):
*        Remove setting high-water mark (it's already set by ems1Stok)
*        - this allow emsRenew to work properly.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

#include "ems1.h"                    /* EMS1_ Internal functions */
#include "ems_par.h"                 /* EMS_ public constants */
#include "ems_sys.h"                 /* EMS_ private constants */

/*  Global Variables: */
#include "ems_toktb.h"               /* Message token table */

void ems1Ktok ( void ) {

   TRACE("ems1Ktok");

/*  Clear the token table at the current context. */
   if ( tokmrk > 1 ) {
      tokcnt[ tokmrk ] = tokcnt[ tokmrk - 1 ];
   } else {
      tokcnt[ tokmrk ] = 0;
   }
   return;
}
