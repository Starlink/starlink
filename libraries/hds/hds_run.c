#if HAVE_CONFIG_H
#  include <config.h>
#endif

#include "sae_par.h"
#include "ems.h"                 /* EMS error reporting routines            */
#include "dat_par.h"             /* Global definitions for HDS              */
#include "dat_err.h"             /* DAT__ error code definitions            */
#include "f77.h"                 /* Fortran interface                       */

/* Note that this routine is obselete and does NOT have a counterpart in    */
/* C interface to HDS                                                       */

/* Function prototypes (these are part of the external interface for HDS).  */
   F77_SUBROUTINE(hds_start)( F77_INTEGER_TYPE(*status) );
   F77_SUBROUTINE(hds_stop)( F77_INTEGER_TYPE(*status) );



F77_SUBROUTINE(hds_run)( SUBROUTINE(subroutine),
                         F77_INTEGER_TYPE(*status ) )
{
/*
*+
*  Name:
*     HDS_RUN

*  Purpose:
*     Run an HDS application subroutine.

*  Language:
*     ANSI C

*  Invocation:
*     CALL HDS_RUN( APP, STATUS )

*  Description:
*     This routine starts up HDS, runs an application subroutine which
*     uses HDS, and then closes HDS down again.

*  Arguments:
*     APP = SUBROUTINE (Given)
*        The subroutine to be executed.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     The application subroutine to be executed should have a single
*     integer STATUS argument and must be declared in an EXTERNAL
*     statement in the routine which calls HDS_RUN.

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK, RAL)
*     BKM:  B.K. McIlwrath    (STARLINK, RAL)
*     {enter_new_authors_here}

*  History:
*     04-APR-1991 (RFWS):
*        Added prologue and error reporting and made portable.
*     19-MAR-2001 (BKM):
*        Convert to use consistent F77 macros and mark as obselete.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

/* Check the inherited global status. */
      if ( *status != SAI__OK ) return;

/* Start up HDS, call the application and close HDS down.
   We do not need to lock because we know that internally these
   are resolved as C functions with a Fortran API. */
      F77_CALL(hds_start)( status );
      (*subroutine)( status );
      F77_CALL(hds_stop)( status );

/* If an error occurred, then report contextual information.  */
      if ( *status != SAI__OK )
      {
         emsRep( "HDS_RUN_ERR",
                   "HDS_RUN: Error running an HDS application subroutine.",
                    status );
      }

/* Exit the routine */
}
