/*+
 *  Name:
 *     emsSyser

 *  Purpose:
 *     Assign a System error message to a token.

 *  Language:
 *     Starlink ANSI C

 *  Invocation:
 *     emsSyser( token, systat )

 *  Description:
 *     This function replaces the Error Message 
 *     Service routine EMS_SYSER (written in Fortran).

 *  Arguments:
 *     token = const char * (Given)
 *        The message token to be associated with the error text.
 *     systat = int (Given)
 *        The Operating System status value.

 *  Authors:
 *     PCTR: P.C.T. Rees (STARLINK)
 *     AJC: A.J. Chipperfield (STARLINK)
 *     RTP: R T Platon (STARLINK)
 *     {enter_new_authors_here}

 *  History:
 *     6-JUN-1990 (PCTR):
 *        Original version, coded as a C macro function.
 *     15-AUG-1990 (PCTR):
 *        C function code.
 *     21-JUN-1991 (PCTR):
 *        Made all given character strings type "const".
 *     13-MAY-1999 (AJC):
 *        Renamed from ems_syser_c
 *     14-FEB-2001 (RTP)
 *        Rewritten in C without the Fortran call to EMS_SYSER
 *     15-MAR-2001 (AJC):
 *        Properly import token name
 *     13-AUG-2001 (AJC):
 *        Remove unused variables
 *     {enter_further_changes_here}

 *  Bugs:
 *     {note_any_bugs_here}

 *-
 */

         
/* Include Statements: */
#include <string.h>                    /* String handling library functions */
#include "ems_par.h"                   /* ems_ public constant definitions */
#include "ems_sys.h"                   /* ems_ private macro definitions */
#include "ems.h"                       /* ems_ function prototypes */
#include "ems1.h"                      /* ems_ internal function prototypes */

/* Function Definitons: */
void emsSyser( const char *token, int systat ){
   int meslen;
   char mesval[EMS__SZMSG+1];

   TRACE("emsSyser");

   ems1Serr( mesval, EMS__SZTOK, &systat );

   meslen = strlen( mesval );
   if ( meslen == 0 ) {
      sprintf( mesval, "No translation for System status value %d", systat );
   }

   ems1Stok( token, mesval );

   return;
}
