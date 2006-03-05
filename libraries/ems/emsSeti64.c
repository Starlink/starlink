/*+
 *  Name:
 *     emsSeti64

 *  Purpose:
 *     Assign a signed 64bit int value to a message token (concise).

 *  Language:
 *     Starlink ANSI C

 *  Invocation:
 *     emsSeti64( token, ivalue )

 *  Description:
 *     The given 64-bit int value is encoded using a concise format and the
 *     result assigned to the named message token. If the token is already 
 *     defined the result is appended to the existing token value.
 *     No Fortran-callable interface is provided.

 *  Arguments:
 *     token = const char * (Given)
 *        The message token name.
 *     ivalue = int64_t (Given)
 *        The 64-bit integer value to be assigned to the message token.

 *  Authors:
 *     PCTR: P.C.T. Rees (STARLINK)
 *     AJC: A.J. Chipperfield (STARLINK)
 *     RTP: R.T.Platon (STARLINK)
 *     TIMJ: Tim Jenness (JAC, Hawaii)
 *     {enter_new_authors_here}

 *  History:
 *     6-JUN-1990 (PCTR):
 *        Original version, coded as a C macro function.
 *     14-AUG-1990 (PCTR):
 *        C function code.
 *     21-JUN-1991 (PCTR):
 *        Made all given character strings type "const".
 *     13-MAY-1999 (AJC):
 *        Renamed from ems_setl_c
 *     14-FEB-2001 (RTP):
 *        Renamed from ems_setl_c
 *      3-MAR-2001 (AJC):
 *        Import token name
 *      3-MAR-2006 (TIMJ):
 *        Copy from emsSeti
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
void emsSeti64( const char *token, int64_t ivalue ){

/* Local Type Definitions: */
   char str[EMS__SZTOK+1];

   TRACE("emsSeti64");
   DEBUG("emsSeti64","emsSeti64: %lld", ivalue);

/*  Construct the message token string. */
   sprintf( str, "%lld", ivalue );
   ems1Stok( token, str );
 
   return;
}
