/*
*+
*  Name:
*     EMS1MPUSH

*  Purpose:
*     Push a new context for the token table.

*  Language:
*     Starlink ANSI C

*  Invocation:
*     ems1Mpush

*  Description:
*     Set the token table indices for a new context.

*  Authors:
*     SLW: Sid Wright  (UCL)
*     PCTR: P.C.T. Rees (STARLINK)
*     RTP: R.T. Platon (STARLINK)
*     {enter_new_authors_here}

*  History:
*     14-APR-1984 (SLW):
*        Original FORTRAN version.
*     14-FEB-2001 (RTP)
*        Rewritten in C based on the Fortran routine EMS1_MPUSH
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

/*  Global Constants: */
#include "ems1.h"                    /* EMS_ Internal functions */
#include "ems_par.h"                 /* EMS_ public constants */
#include "ems_sys.h"                 /* EMS_ private constants */

/*  Global Variables: */
#include "ems_toktb.h"               /* Message token table */

void ems1Mpush( void ) {

   TRACE("ems1Mpush");

/*  Check for maximum number of message context levels. */
   if ( toklev < EMS__MXLEV ) {

/*     OK to push context. */
      toklev++;
      tokmrk++;
      tokcnt[ tokmrk ] = tokhiw[ tokmrk - 1 ];
      tokhiw[ tokmrk ] = tokhiw[ tokmrk - 1 ];
   } else {

/*     Context stack full, so increment TOKLEV only. */
      toklev++;
   }
   return; 
}
