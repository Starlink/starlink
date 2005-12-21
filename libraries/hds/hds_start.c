#if HAVE_CONFIG_H
#  include <config.h>
#endif

#include "f77.h"                 /* F77 <-> C interface macros              */
#include "ems.h"                 /* EMS error reporting routines            */
#include "hds1.h"                /* Global definitions for HDS              */
#include "str.h"                 /* Character string import/export macros   */
#include "dat_err.h"             /* DAT__ error code definitions            */

#include "rec.h"
#include "dat1.h"                /* Function prototypes                     */

/* Note that this routine is obselete and does NOT have a counterpart in    */
/* C interface to HDS                                                       */

F77_SUBROUTINE(hds_start)( F77_INTEGER_TYPE(*status) )
{
/*
*+
*  Name:
*     HDS_START

*  Purpose:
*     Start up HDS.

*  Language:
*     ANSI C

*  Invocation:
*     CALL HDS_START( STATUS )

*  Description:
*     This routine should be called to start up HDS prior to making
*     calls to other HDS routines. It ensures that HDS is active,
*     returning without action if it is already active.

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     This routine is now obsolete, as HDS starts itself up automatically
*     when required.

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK, RAL)
*     BKM:  B.K. McIlwrath    (STARLINK, RAL)
*     {enter_new_authors_here}

*  History:
*     4-APR-1991 (RFWS):
*        Added prologue and made portable.
*     24-FEB-1992 (RFWS):
*        Modified to call HDS1_INIT internally.
*     19-MAR-2001 (BKM):
*        Convert to use consistent F77 macros and remove tabs.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

/* Check the inherited global status */
      if ( !_ok( *status ) ) return;
      hds_gl_status = DAT__OK;

/* Initialise HDS  */
      dat1_init();

/* If an error occurred, then report contextual information */
      if ( !_ok( hds_gl_status ) )
      {
         ems_rep_c( "HDS_START_ERR",
                    "HDS_START: Error activating the Hierarchical Data System \
                     (HDS).",&hds_gl_status );
      }
           
/* Exit the routine */
      *status = hds_gl_status;
}
