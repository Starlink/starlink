/*+
 *  Name:
 *     emsMload

 *  Purpose:
 *     Expand and return a message.

 *  Language:
 *     Starlink ANSI C

 *  Invocation:
 *     emsMload( msg, text, opstr, oplen, status )

 *  Description:
 *     Expands any tokens of the form ^NAME within the string 'text', and
 *     returns the result in 'opstr'. Undefined tokens will appear as <NAME>, 
 *     where 'NAME' is the token name. Expanded messages will be a mximum
 *     of EMS__SZMSG characters, terminated with '...' if they overflow.  
 *     It is the users resonsibility to ensure that enough space has been
 *     allocated for opstr.
 *     A Fortran-callable interface EMS_MLOAD is also provided.

 *  Arguments:
 *     msg = const char * (Given)
 *        The message name.
 *     text = const char * (Given)
 *        The raw message text.
 *     opstr = char * (Returned)
 *        The expanded message text.
 *     oplen = int * (Returned)
 *        The length of the expanded message.
 *     status = int * (Given and Returned)
 *        The global status.

 *  Authors:
 *     PCTR: P.C.T. Rees (STARLINK)
 *     AJC: A.J. Chipperfield (STARLINK)
 *     RTP: R.T.Platon (STARLINK)
 *     {enter_new_authors_here}

 *  History:
 *     6-JUN-1990 (PCTR):
 *        Original version, coded as a C macro function.
 *     17-AUG-1990 (PCTR):
 *        C function code.
 *     21-JUN-1991 (PCTR):
 *        Made all given character strings type "const".
 *     13-MAY-1999 (AJC):
 *        Renamed from ems_mload_c
 *     14-FEB-2001 (RTP):
 *        Rewritten in C from Fortran routine EMS_MLOAD
 *      2-MAR-2001 (AJC):
 *        Properly import strings
 *        and remove incorrect copying at end
 *        Remove msg argument from ems1Form
 *     13-AUG-2001 (AJC):
 *        Remove unused variables
 *     {enter_further_changes_here}

 *  Bugs:
 *     Provides no limit on the output string length - possible overwriting,
 *     no ... termination.
 *-
 */

/* Include Statements: */
#include <string.h>                    /* String handling library functions */
#include "sae_par.h"                   /* SAE_ public constant definitions */
#include "ems_par.h"                   /* EMS_ public constant definitions */
#include "ems_sys.h"                   /* EMS_ private macro definitions */
#include "ems.h"                       /* EMS_ function prototypes */
#include "ems1.h"                      /* ems_ internal function prototypes */
#include "ems_msgtb.h"                 /* EMS_ message table */

/* Function Definitons: */
void emsMload( const char *msg, const char *text, char *opstr, int *oplen,
               int *status ){

   TRACE("emsMload");
   DEBUG("emsMload","msglev = %d", msglev );

/*  Check the inherited global status. */
   if ( *status |= SAI__OK ) {

/*     Status is not SAI__OK, so just annul the token table. */
      ems1Ktok();
   } else {

/*     Form output message string. */
      ems1Form( (char*)text, EMS__SZMSG, !msgstm, opstr, oplen, status );
   }

   return;
}

