#include "hds1_feature.h"	 /* Define feature-test macros, etc.	    */
#include "hds1.h"		 /* Global definitions for HDS		    */
#include "rec.h"		 /* Public rec_ definitions		    */
#include "rec1.h"		 /* Internal rec_ definitions		    */
#include "dat_err.h"		 /* DAT__ error code definitions	    */

   int rec_get_rid( const struct HAN *han, struct RID *rid )
   {
/*+									    */
/* Name:								    */
/*    rec_get_rid							    */

/* Purpose:								    */
/*    Obtain a record ID for a record.					    */

/* Invocation:								    */
/*    rec_get_rid( han, rid )						    */

/* Description:								    */
/*    This function returns a record ID for a record whose handle is	    */
/*    supplied.								    */

/* Parameters:								    */
/*    const struct HAN *han						    */
/*       Pointer to a HAN structure containing a record handle.		    */
/*    struct RID *rid							    */
/*       Pointer to a RID structure in which the record ID will be	    */
/*	 returned.							    */

/* Returned Value:							    */
/*    int rec_get_rid							    */
/*	 The global status value current on exit.			    */

/* Authors:								    */
/*    RFWS: R.F. Warren-Smith (STARLINK)				    */
/*    {@enter_new_authors_here@}					    */

/* History:								    */
/*    24-APR-1991 (RFWS):						    */
/*       Added prologue and error handling and tidied.			    */
/*    {@enter_changes_here@}						    */

/* Bugs:								    */
/*    {@note_any_bugs_here@}						    */

/*-									    */

/*.									    */

/* Check the inherited global status.					    */
      if ( !_ok( hds_gl_status ) ) return hds_gl_status;

/* Extract the Record ID from the handle.				    */
      *rid = han->rid;

/* Return the current global status value.				    */
      return hds_gl_status;
   }
