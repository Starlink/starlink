#include "hds1_feature.h"	 /* Define feature-test macros, etc.	    */
#include <stddef.h>
#include "hds1.h"		 /* Global definitions for HDS		    */
#include "rec.h"		 /* Public rec_ definitions		    */
#include "rec1.h"		 /* Internal rec_ definitions		    */
#include "dat_err.h"		 /* DAT__ error code definitions	    */

   int rec_adopt_record( const struct HAN *han, const struct HAN *par )
   {
/*+									    */
/* Name:								    */
/*    rec_adopt_record							    */

/* Purpose:								    */
/*    Change the parent of a record.					    */

/* Invocation:								    */
/*    rec_adopt_record( han, par )					    */

/* Description:								    */
/*    This function changes the ownership of a record by setting its parent */
/*    record.								    */

/* Parameters:								    */
/*    const struct HAN *han						    */
/*       Pointer to a HAN structure containing a handle for the record to   */
/*	 be adopted.							    */
/*    const struct HAN *par						    */
/*       Pointer to a HAN structure containing a handle for the record	    */
/*	 which is to become the new parent.				    */

/* Returned Value:							    */
/*    int rec_adopt_record						    */
/*	 The global status value current on exit.			    */

/* Authors:								    */
/*    RFWS: R.F. Warren-Smith (STARLINK)				    */
/*    {@enter_new_authors_here@}					    */

/* History:								    */
/*    24-APR-1991 (RFWS):						    */
/*	 Added prologue and error handling, made portable and tidied.	    */
/*    14-JUNE-1991 (RFWS):						    */
/*	 Changed access mode to 'U' in rec_locate_block.		    */
/*    {@enter_further_changes_here@}					    */

/* Bugs:								    */
/*    {@note_any_bugs_here@}						    */

/*-									    */

/* Local Variables:							    */
      struct RCL rcl;		 /* Record Control Label		    */
      unsigned char *cdom;	 /* Pointer to Control Domain		    */
      unsigned char *lrb;	 /* Pointer to Logical Record Block	    */

/*.									    */

/* Check the inherited global status.					    */
      if ( !_ok( hds_gl_status ) ) return hds_gl_status;

/* Locate the Logical Record Block containing the record to be adopted.	    */
      rec_locate_block( han->slot, han->rid.bloc, 'U', &lrb );

/* Obtain a pointer to the Control Domain and unpack the Record Control	    */
/* Label.								    */
      cdom = lrb + REC__SZCBM + ( han->rid.chip * REC__SZCHIP );
      rec1_unpack_rcl( cdom, &rcl );

/* Modify the parent record ID and re-pack the Record Control Label.	    */
      rcl.parent = par->rid;
      rec1_pack_rcl( &rcl, cdom );

/* Release the Logical Record Block.					    */
      if ( lrb != NULL ) rec_release_block( han->slot, han->rid.bloc );

/* Return the current global status value.				    */
      return hds_gl_status;
   }
