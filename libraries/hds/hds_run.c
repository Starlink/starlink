#include "hds1_feature.h"	 /* Define feature-test macros, etc.	    */
#include "f77.h"		 /* F77 <-> C interface macros		    */
#include "ems.h"		 /* EMS error reporting routines	    */
#include "hds1.h"		 /* Global definitions for HDS		    */
#include "str.h"		 /* Character string import/export macros   */
#include "dat_err.h"		 /* DAT__ error code definitions	    */

/* Function prototypes (these are part of the external interface for HDS).  */
   F77_INTEGER_FUNCTION(hds_start)( int *status );
   F77_INTEGER_FUNCTION(hds_stop)( int *status );


   F77_INTEGER_FUNCTION(hds_run)
		       ( void (*subroutine)( int * ),
			 int *status )
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
*     {enter_new_authors_here}

*  History:
*     4-APR-1991 (RFWS):
*        Added prologue and error reporting and made portable.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

/*.									    */

/* Check the inherited global status.					    */
      if ( !_ok( *status ) ) return *status;

/* Start up HDS, call the application and close HDS down.		    */
      F77_CALL(hds_start)( status );
      (*subroutine)( status );
      F77_CALL(hds_stop)( status );

/* If an error occurred, then report contextual information.		    */
      if ( !_ok( *status ) )
      {
         ems_rep_c( "HDS_RUN_ERR",
	            "HDS_RUN: Error running an HDS application subroutine.",
		    status );
      }

/* Exit the routine.							    */
      return *status;
   }
