/*+
 *  Name:
 *     emsAnnul

 *  Purpose:
 *     Annul the contents of the current error context.

 *  Language:
 *     Starlink ANSI C

 *  Invocation:
 *     emsAnnul( status )

 *  Description:
 *     This function provides a C interface for the Error Message 
 *     Service routine EMS_ANNUL (written in Fortran).

 *  Arguments:
 *     status = int * (Returned)
 *        The global status value.

 *  Authors:
 *     PCTR: P.C.T. Rees (STARLINK)
 *     AJC: A.J. Chipperfield (STARLINK)
 *     RTP: R.T.Platon (STARLINK)
 *     {enter_new_authors_here}

 *  History:
 *     6-JUN-1990 (PCTR):
 *        Original version, coded as a C macro function.
 *     9-AUG-1990 (PCTR):
 *        C function code.
 *     13-MAY-1999 (AJC):
 *        Renamed from ems_annul_c
 *     14-FEB-2001 (RTP):
 *        Rewritten in C from Fortran routine EMS_ANNUL
 *     {enter_further_changes_here}

 *  Bugs:
 *     {note_any_bugs_here}

 *-
 */ 

/* Include Statements: */
#include "sae_par.h"
#include "ems_par.h"                   /* ems_ public constant definitions */
#include "ems_sys.h"                   /* ems_ private macro definitions */
#include "ems.h"                       /* ems_ function prototypes */
#include "ems1.h"                      /* ems_ internal function prototypes */

/* Function Definitions: */
void emsAnnul( int *status ){
   int level;

   TRACE ( "emsAnnul" );

   emsLevel ( &level );
   if ( level <= EMS__MXLEV ) {
      ems1Kerr();
      ems1Ktok();
   }
   *status = SAI__OK;

}
