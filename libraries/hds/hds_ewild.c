#include "hds1_feature.h"	 /* Define feature-test macros, etc.	    */
#include "f77.h"		 /* F77 <-> C interface macros		    */
#include "ems.h"		 /* EMS error reporting routines	    */
#include "hds1.h"		 /* Global definitions for HDS		    */
#include "rec.h"		 /* Public rec_ definitions		    */
#include "dat1.h"		 /* Internal dat_ definitions		    */
#include "dat_err.h"		 /* DAT__ error code definitions	    */

   F77_INTEGER_FUNCTION(hds_ewild)( INTEGER(IWLD),
				    INTEGER(STATUS) )
   {
/*
*+
*  Name:
*     HDS_EWILD

*  Purpose:
*     End a wild-card search for HDS container files.

*  Language:
*     ANSI C

*  Invocation:
*     CALL HDS_EWILD( IWLD, STATUS )

*  Description:
*     The routine ends a wild-card search for HDS container files begun
*     by HDS_WILD, and annuls the wild-card search context used. It
*     should be called after a wild-card search is complete in order to
*     release the resources used.

*  Arguments:
*     IWLD = INTEGER (Given and Returned)
*        Identifier for the wild-card search context to be annulled, as
*        returned by HDS_WILD. A value of DAT__NOWLD is returned (as
*        defined in the include file DAT_PAR).
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     This routine attempts to execute even if STATUS is set on entry,
*     although no further error report will be made if it subsequently
*     fails under these circumstances. In particular, it will fail if
*     the identifier supplied is not initially valid, but this will
*     only be reported if STATUS is set to SAI__OK on entry.

*  Copyright:
*     Copyright (C) 1992 Science & Engineering Research Council

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK, RAL)
*     {enter_new_authors_here}

*  History:
*     30-OCT-1992 (RFWS):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

/* Arguments Given and Returned:					    */
      GENPTR_INTEGER(IWLD)

/* Status:								    */
      GENPTR_INTEGER(STATUS)

/* Local Variables:							    */
      struct WLD *context;	 /* Pointer to wild-card search context	    */

/*.									    */

/* Begin a new error reporting environment.				    */
      hds_gl_status = (INT) *STATUS;
      ems_begin_c( &hds_gl_status );

/* Copy the wild-card search context identifier and annul the context.	    */
      context = (struct WLD *) *IWLD;
      rec_end_wild( &context );

/* Return a null context identifier.					    */
      *IWLD = DAT__NOWLD;

/* If an error occurred, then report a contextual error message.	    */
      if ( !_ok( hds_gl_status ) )
      {
         ems_rep_c( "HDS_EWILD_ERR",
	            "HDS_EWILD: Error ending a wild-card search for HDS \
container files.",
		    &hds_gl_status );
      }

/* End the error reporting environment.					    */
      ems_end_c( &hds_gl_status );

/* Return the global status.						    */
      *STATUS = (F77_INTEGER_TYPE) hds_gl_status;
      return *STATUS;
   }
