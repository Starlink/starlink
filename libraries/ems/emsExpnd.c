/*+
 *  Name:
 *     emsExpnd

 *  Purpose:
 *     Expand and return a message.

 *  Language:
 *     Starlink ANSI C

 *  Invocation:
 *     emsExpnd( text, opstr, maxlen, oplen, status )

 *  Description:
 *     Expands any tokens of the form ^NAME within the string 'text' and
 *     returns the result in 'opstr'. Undefined tokens will appear as <TOKEN>,
 *     where 'NAME' is the token name. If the expanded message exceeds the
 *     given maximum length 'maxlen', opstr will be terminated with '...'.
 *     It is the users resonsibility to ensure that enough space has been
 *     allocated for opstr.
 *     A Fortran-callable interface EMS_EXPND is also provided.

 *  Arguments:
 *     text = const char * (Given)
 *        The raw message text.
 *     opstr = char * (Returned)
 *        The expanded message text.
 *     maxlen = const int (Given)
 *        The maximum length for the expanded string
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
 *     12-MAR-2001 (AJC):
 *        Original version, modified from emsMload (now deprecated)
 *     13-AUG-2001 (AJC):
 *        Remove unused variables
 *     {enter_further_changes_here}

 *  Bugs:
 *     {note_any_bugs_here}
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
void emsExpnd( const char *text, char *opstr, const int maxlen, int *oplen,
               int *status ){

   TRACE("emsExpnd");
   DEBUG("emsExpnd","msglev = %d", msglev );

/*  Check the inherited global status. */
   if ( *status |= SAI__OK ) {

/*     Status is not SAI__OK, so just annul the token table. */
      ems1Ktok();
   } else {

/*     Form output message string. */
      ems1Form( (char*)text, maxlen, !msgstm, opstr, oplen, status );
   }

   return;
}
