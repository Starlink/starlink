/*+
 *  Name:
 *     emsLevel

 *  Purpose:
 *     Inquire the current error context level.

 *  Language:
 *     Starlink ANSI C

 *  Invocation:
 *     emsLevel( level )

 *  Description:
 *     This function provides a C interface for the Error Message 
 *     Service routine EMS_LEVEL (written in Fortran).

 *  Arguments:
 *     level = int * (Returned)
 *        The error context level.

 *  Authors:
 *     PCTR: P.C.T. Rees (STARLINK)
 *     AJC: A.J. Chipperfield (STARLINK)
 *     RTP: R.T.Platon (STARLINK)
 *     {enter_new_authors_here}

 *  History:
 *     8-NOV-1990 (PCTR):
 *        Original version.
 *     13-MAY-1999 (AJC):
 *        Renamed from ems_level_c
 *     14-FEB-2001 (RTP):
 *        Rewritten in C from Fortran routine EMS_LEVEL
 *     {enter_further_changes_here}

 *  Bugs:
 *     {note_any_bugs_here}
 */

/* Include Statements: */
#include "ems_par.h"                   /* ems_ public constant definitions */
#include "ems.h"                       /* ems_ function prototypes */
#include "ems_sys.h"                   /* ems_ private macro definitions */
#include "ems_msgtb.h"                 /* ems_ message table */

/* Function Definitions: */
void emsLevel( int *level ){

   TRACE("emsLevel");
   DEBUG("emsLevel","msglev = %d", msglev );

/*  Load the returned value of LEVEL. */
   *level = msglev;

   return;
}
