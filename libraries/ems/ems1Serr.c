/*+
 *  Name:
 *     ems1Serr

 *  Purpose:
 *     Get the error message associated with a system error code.
 *     This is the Linux version.

 *  Language:
 *     Starlink ANSI C

 *  Invocation:
 *     ems1Serr( errstr, errlen, ernum_p)

 *  Description:
 *     This C function uses the system function strerror

 *  Arguments:
 *     errstr = char* (Returned)
 *        The System error message string.
 *     errlen = int (Given)
 *        The maximum length of errstr.
 *     errnum = int* (Given)
 *        A pointer to the error number.

 *  Authors:
 *     PCTR: P.C.T. Rees (STARLINK)
 *     BKM: B.K.McIlwrath (STARLINK)
 *     RTP: R.T.Platon (STARLINK)
 *     AJC: A.J.Chipperfield (STARLINK)
 *     {enter_new_authors_here}

 *  History:
 *     23-MAY-1990 (PCTR):
 *        Original version.
 *     12-AUG-1994 (BKM):
 *        Change name from EMS1_GERR to EMS1_SERR
 *     20-FEb-2001 (RTP):
 *        Change name from EMS1_SERR to ems1Serr
 *     30-APR-2003 (AJC):
 *        Use strerror not sys_errlist
 *     {enter_further_changes_here}

 *  Bugs:
 *     {note_any_bugs_here}

 *- */

#include <string.h>
#include "ems1.h"      /* EMS1_ internal functions */

/*  Function Definition: */
void ems1Serr( char *errstr, int errlen, int *errnum ) {

/*  Local Variables: */
   char *strerr;                  /* Pointer for system error message string */

/*    Load string. */
   strerr = ( *errnum>=0 )? strerror( *errnum ) : NULL;
   if( strerr != NULL ) {
      (void)strncpy( errstr, strerr, errlen );
   } else {
      *errstr = '\0';
   }
   return;
}
