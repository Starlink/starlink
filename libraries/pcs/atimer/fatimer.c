/*
*+
*  Name:
*     FATIMER

*  Purpose:
*     A Fortran callable interface to the ATIMER C routines

*  Language:
*     C

*  Description:
*     Provides a Fortran callable interface to ATIMER_SETTIMR
*     and ATIMER_CANTIM which set and cancel millisecond timers.
*     A C routine has to be written as the handler routine, specified
*     in the SSETTIMR call, which is called directly by the ATIMER system. 
*     The C handler may be written as follws to call a handler written
*     in Fortran:
*
*        #include "f77.h"
*
*        extern void F77_EXTERNAL_NAME(fhandlr)( INTEGER(id) );
*
*        F77_SUBROUTINE(chandlr)( int id )
*        {
*        F77_CALL(fhandlr)( INTEGER_ARG(&id) );
*        }

*  Authors:
*     AJC: A J Chipperfield (STARLINK)
*     {enter_new_authors_here}

*  History:
*     27-JUN-1994 (AJC):
*        Original version.
*     18-SEP-1995 (AJC):
*        Correct declaration of handler argument in settimr.
*     {enter_changes_here}
*-
*/
      
/* Include Statements: */
#include "sae_par.h"
#include "f77.h"
#include "atimer.h"

/*
*+
*  Name:
*     FATIMER_CANTIM( ID, STATUS )

*  Purpose:
*     A Fortran callable interface to the ATIMER_CANTIM C routine

*  Language:
*     C

*  Invocation:
*     CALL FATIMER_CANTIM( ID, STATUS )

*  Description:
*     Cancels the ATIMER timer with the specified id.
*     The value of the id is obtained and specified in a call to 
*     atimer_settimr.

*  Arguments:
*     ID = INTEGER (Given)
*        The timer id as given in the associated ATIMER_SETTIMR call.
*     STATUS = INTEGER (Given and returned)
*        The global status.

*  [optional_subroutine_items]...
*  Authors:
*     AJC: A J Chipperfield (STARLINK)
*     {enter_new_authors_here}

*  History:
*     27-JUN-1994 (AJC):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

/* Function Definition: */
F77_SUBROUTINE(fatimer_cantim)( INTEGER(id), INTEGER(status) )
{
/* Exit if bad given status 
*/
   if ( *status != SAI__OK ) return;

   atimer_cantim( *id, status );

   return;
}

/*
*+
*  Name:
*     FATIMER_SETTIMR( DELAY, ID, HANDLER, STATUS )

*  Purpose:
*     A Fortran callable interface to the ATIMER_settimr C routine

*  Language:
*     C

*  Invocation:
*     CALL FATIMER_SETTIMR( DELAY, ID, HANDLER, STATUS )

*  Description:
*     Cancels the ATIMER timer with the specified id.
*     The value of the id is obtained and specified in a call to 
*     atimer_settimr.

*  Arguments:
*     DELAY = INTEGER (Given)
*        The delay time in milliseconds
*     ID = INTEGER (Given)
*        The timer id as given in the associated ATIMER_SETTIMR call.
*     HANDLER = EXTERNAL (Given)
*        The name of the rouitne to be called with the timer ID as the
*        sole argument when the specified delay time has elapsed.
*     STATUS = INTEGER (Given and returned)
*        The global status.

*  Authors:
*     AJC: A J Chipperfield (STARLINK)
*     {enter_new_authors_here}

*  History:
*     27-JUN-1994 (AJC):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/
      
/* Function Definition: */
F77_SUBROUTINE(fatimer_settimr)( INTEGER(delay), INTEGER(id), 
                                 void (*handler)(), INTEGER(status) )
{

/* Exit if bad given status 
*/
   if ( *status != SAI__OK ) return;

   atimer_settimr( *delay, *id, handler, status );

   return;
}

