/*+
 *  Name:
 *     emsBegin

 *  Purpose:
 *     Begin a new error reporting environment.

 *  Language:
 *     Starlink ANSI C

 *  Invocation:
 *     emsBegin( status )

 *  Description:
 *     This function provides a C interface for the Error Message 
 *     Service routine EMS_BEGIN (written in Fortran).

 *  Arguments:
 *     status = int * (Returned)
 *        The global status value.

 *  Authors:
 *     PCTR: P.C.T. Rees (STARLINK)
 *     AJC: A.J. Chipperfield (STARLINK)
 *     RTP: R.T.Platon (STARLINK)
 *     {enter_new_authors_here}

 *  History:
 *     8-NOV-1990 (PCTR):
 *        Original version.
 *     13-MAY-1999 (AJC):
 *        Renamed from ems_begin_c
 *     14-FEB-2001 (RTP):
 *        Rewritten in C from Fortran routine EMS_BEGIN
 *     19-MAR-2001 (AJC):
 *        Correct testing and resetting of status - and comment.
 *     {enter_further_changes_here}

 *  Bugs:
 *     {note_any_bugs_here}

 *-
 */

/* Include Statements: */
#include "sae_par.h"
#include "ems_par.h"                   /* ems_ public constant definitions */
#include "ems.h"                       /* ems_ function prototypes */
#include "ems_sys.h"                   /* ems_ private macro definitions */
#include "ems_msgtb.h"                 /* Message token table */

/* Function Definitions: */
void emsBegin( int *status ){

   int istat, lstat;

   TRACE("emsBegin");
   DEBUG("emsBegin","msglev = %d", msglev );

   if (*status != SAI__OK ) {
/*  If status is not OK,    
*   Check if there are any error messages pending output */
      emsStat( &istat );

/*  If the last reported status is SAI__OK, then no error messages are pending
*   so report an error to this effect */
      if ( istat == SAI__OK ) {
          emsMark();
          lstat = *status;
          emsRep( "EMS_BEGIN_NOREP",
            "ERR_/EMS_BEGIN: "
            "STATUS set with no error report (improper use of EMS).",
            &lstat );
          emsRlse();
      }
   }
   if ( msglev <= EMS__MXLEV ) msgbgs[ msglev ] = *status;
   emsMark();
   *status = SAI__OK;

   return;
}
