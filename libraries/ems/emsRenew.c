/*+
 *  Name:
 *     emsRenew

 *  Purpose:
 *     Renew any annulled message tokens in the current context.

 *  Language:
 *     Starlink ANSI C

 *  Invocation:
 *     emsRenew( )

 *  Description:
 *     This function provides a C interface for the Error Message 
 *     Service routine EMS_RENEW (written in Fortran).

 *  Authors:
 *     PCTR: P.C.T. Rees (STARLINK)
 *     AJC: A.J. Chipperfield (STARLINK)
 *     RTP: R.T.Platon (STARLINK)
 *     {enter_new_authors_here}

 *  History:
 *     21-JUN-1991 (PCTR):
 *        Original version.
 *     13-MAY-1999 (AJC):
 *        Renamed from ems_renew_c
 *     14-FEB-2001 (RTP):
 *        Rewritten in C from Fortran routine EMS_RENEW
 *     {enter_further_changes_here}

 *  Bugs:
 *     {note_any_bugs_here}

 *- */

/* Include Statements: */
#include "ems_par.h"                   /* EMS_ public constant definitions */
#include "ems.h"                       /* EMS_ function prototypes */
#include "ems_sys.h"                   /* EMS_ private macro definitions */
#include "ems_toktb.h"                 /* EMS_ token table */
             
/* Function Definitons: */
void emsRenew( void ){

   TRACE( "emsRenew");

/*  Assign the token count to the token high water mark. */
   tokcnt[ tokmrk ] = tokhiw[ tokmrk ];

   return;
}
