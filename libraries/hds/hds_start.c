#include "hds1_feature.h"	 /* Define feature-test macros, etc.	    */
#include "f77.h"		 /* F77 <-> C interface macros		    */
#include "ems.h"		 /* EMS error reporting routines	    */
#include "hds1.h"		 /* Global definitions for HDS		    */
#include "rec.h"		 /* Public rec_ definitions		    */
#include "dat1.h"		 /* Internal dat_ definitions		    */
#include "dat_err.h"		 /* DAT__ error code definitions	    */

   F77_INTEGER_FUNCTION(hds_start)
                       ( int *status )
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
*     {enter_new_authors_here}

*  History:
*     4-APR-1991 (RFWS):
*        Added prologue and made portable.
*     24-FEB-1992 (RFWS):
*        Modified to call HDS1_INIT internally.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

/*.									    */

/* Check the inherited global status.					    */
      if ( !_ok( *status ) ) return *status;
      hds_gl_status = DAT__OK;

/* Initialise HDS.							    */
      dat1_init( );

/* If an error occurred, then report contextual information.		    */
      if ( !_ok( hds_gl_status ) )
      {
         ems_rep_c( "HDS_START_ERR",
		    "HDS_START: Error activating the Hierarchical Data System \
(HDS).",
		    &hds_gl_status );
      }
	           
/* Exit the routine, returning the current global status value.		    */
      *status = hds_gl_status;
      return *status;
   }
