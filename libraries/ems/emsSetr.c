/*+
 *  Name:
 *     emsSetr

 *  Purpose:
 *     Assign a REAL value to a message token (concise).

 *  Language:
 *     Starlink ANSI C

 *  Invocation:
 *     emsSetr( token, rvalue )

 *  Description:
 *     The given float value is encoded using a concise format and the result
 *     assigned to the named message token. If the token is already defined,
 *     the result is appended to the existing token value.
 *     A Fortran-callable interface EMS_SETR is also provided.

 *  Arguments:
 *     token = const char * (Given)
 *        The message token name.
 *     rvalue = float (Given)
 *        The REAL value to be assigned to the message token.

 *  Authors:
 *     PCTR: P.C.T. Rees (STARLINK)
 *     AJC: A.J. Chipperfield (STARLINK)
 *     RTP: R.T.Platon (STARLINK)
 *     {enter_new_authors_here}

 *  History:
 *     6-JUN-1990 (PCTR):
 *        Original version, coded as a C macro function.
 *     14-AUG-1990 (PCTR):
 *        C function code.
 *     21-JUN-1991 (PCTR):
 *        Made all given character strings type "const".
 *     13-MAY-1999 (AJC):
 *        Renamed from ems_setr_c
 *     14-FEB-2001 (RTP):
 *        Rewritten to remove the Fortran call to EMS_SETR,
 *     13-MAR-2001 (AJC):
 *        Properly import token name
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
void emsSetr( const char *token, float rvalue ){
   char str[EMS__SZTOK];

   TRACE("emsSetr");

   sprintf( str, "%g", rvalue );
   ems1Stok( token, str );
 
   return;
}
