#include "hds1_feature.h"	 /* Define feature-test macros, etc.	    */
/*+ rec_get_rcl.c -*/

#include <stddef.h>

#include "hds1.h"		 /* Global definitions for HDS		    */
#include "rec.h"		 /* Public rec_ definitions		    */
#include "rec1.h"		 /* Internal rec_ definitions		    */
#include "dat_err.h"		 /* DAT__ error code definitions	    */

   int rec_get_rcl( const struct HAN *han, struct RCL *rcl )
   {
/*+									    */
/* Name:								    */
/*    rec_get_rcl							    */

/* Purpose:								    */
/*    Read a Record Control Label from a container file record.		    */

/* Invocation:								    */
/*    rec_get_rcl( han, rcl )						    */

/* Description:								    */
/*    This function reads and unpacks the contents of the Record Control    */
/*    Label held in the Control Domain of a container file record for which */
/*    a handle is supplied. The information is returned in an RCL	    */
/*    structure.							    */

/* Parameters:								    */
/*    const struct HAN *han						    */
/*	 Pointer to a HAN structure containing a handle for the required    */
/*	 record.							    */
/*    struct RCL *rcl							    */
/*       Pointer to an RCL structure in which the unpacked Record Control   */
/*	 Label information will be returned.				    */

/* Returned Value:							    */
/*    int rec_get_rcl							    */
/*	 The global status value current on exit.			    */

/* Authors:								    */
/*    RFWS: R.F. Warren-Smith (STARLINK)				    */
/*    {@enter_new_authors_here@}					    */

/* History:								    */
/*    2-APR-1991 (RFWS):						    */
/*	 Added prologue and changed to unpack the information.		    */
/*    16-APR-1991 (RFWS):						    */
/*       Eliminated unnecessary local variables and improved error	    */
/*	 handling.							    */
/*    {@enter_further_changes_here@}					    */

/* Bugs:								    */
/*    {@note_any_bugs_here@}						    */

/*-									    */

/* Local Variables:							    */
      unsigned char *cdom;	 /* Pointer to LRB Control Domain	    */
      unsigned char *lrb;	 /* Pointer to Logical Record Block	    */

/*.									    */

/* Check the inherited global status.					    */
      if ( !_ok( hds_gl_status ) ) return hds_gl_status;

/* Find the Logical Record Block which contains the required record.	    */
      rec_locate_block( han->slot, han->rid.bloc, 'R', &lrb );

/* Locate the record's Control Domain and unpack the Record Control Label.  */
      cdom = lrb + REC__SZCBM + ( han->rid.chip * REC__SZCHIP );
      rec1_unpack_rcl( cdom, rcl );

/* Release the Logical Record Block.					    */
      if ( lrb != NULL ) rec_release_block( han->slot, han->rid.bloc );

/* Return the current global status value.				    */
      return hds_gl_status;
   }
