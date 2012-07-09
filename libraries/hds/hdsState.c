#if HAVE_CONFIG_H
#  include <config.h>
#endif

/*+ HDS_STATE.C -*/

#include "hds1.h"                /* Global definitions for HDS              */
#include "str.h"                 /* Character string import/export macros   */
#include "dat_err.h"             /* DAT__ error code definitions            */

#include "hds.h"

#define TRUE  1
#define FALSE 0

int
hdsState( int *state,
          int *status)
{
/*
*+
*  Name:
*     HDS_STATE

*  Purpose:
*     Enquire the current state of HDS.

*  Language:
*     ANSI C

*  Invocation:
*     CALL HDS_STATE( STATE, STATUS )

*  Description:
*     This routine returns a logical value indicating whether HDS is
*     currently active or inactive.

*  Arguments:
*     STATE = LOGICAL (Returned)
*        The current state of HDS: .TRUE. for active, .FALSE. for
*        inactive.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK, RAL)
*     BKM:  B.K. McIlwrath    (STARLINK, RAL)
*     {enter_new_authors_here}

*  History:
*     4-APR-1991 (RFWS):
*        Added prologue and made portable.
*     6-AUG-1991 (RFWS):
*        Changed to handle TRUE/FALSE correctly on all machines.
*     13-DEC-2001 (BKM)
*        Convert to a C function with FORTRAN wrapper.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

/*.                                                                         */

/* Check the inherited global status.                                       */
   if ( !_ok( *status ) )
      return *status;

/* Check if HDS is active.                                                  */
      *state = hds_gl_active ? TRUE : FALSE;

/* Exit the routine.                                                        */
      return *status;
}
