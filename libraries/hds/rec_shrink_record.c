#include "hds1_feature.h"	 /* Define feature-test macros, etc.	    */
#include <stddef.h>
#include "hds1.h"		 /* Global definitions for HDS		    */
#include "rec.h"		 /* Public rec_ definitions		    */
#include "rec1.h"		 /* Internal rec_ definitions		    */
#include "dat_err.h"		 /* DAT__ error code definitions	    */

   int rec_shrink_record( const struct HAN *han, int extent )
   {
/*+									    */
/* Name:								    */
/*    rec_shrink_record							    */

/* Purpose:								    */
/*    Shrink a record's Dynamic Domain.					    */

/* Invocation:								    */
/*    rec_shrink_record( han, extent )					    */

/* Description:								    */
/*    This function shrinks the size of a record's Dynamic Domain,	    */
/*    releasing any de-allocated space for re-use. The existing record	    */
/*    contents are retained within that part of the Dynamic Domain which    */
/*    exists after the shrinking operation.				    */

/* Parameters:								    */
/*    const struct HAN *han						    */
/*       Pointer to a HAN structure containing a handle to the record	    */
/*	 which is to be shrunk.						    */
/*    int extent							    */
/*	 The amount (in unsigned chars) by which the size of the record's   */
/*	 Dynamic Domain is to be decreased.				    */

/* Returned Value:							    */
/*    int rec_shrink_record						    */
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
      int bloc;			 /* No. of first block in frame of PDBs	    */
      int chip;			 /* Chip number				    */
      int dlen;			 /* Dynamic Domain length (unsigned chars)  */
      int size;			 /* Size of record in chips		    */
      int spare;		 /* No. spare chips or blocks		    */
      struct RCL rcl;		 /* Record Control Label		    */
      unsigned char *cbm;	 /* Pointer to Chip Bitmap		    */
      unsigned char *cdom;	 /* Pointer to Control Domain		    */
      unsigned char *ddom;	 /* Pointer to Dynamic Domain		    */
      unsigned char *lrb;	 /* Pointer to Logical Record Block	    */

/*.									    */

/* Check the inherited global status.					    */
      if ( !_ok( hds_gl_status ) ) return hds_gl_status;

/* Locate the Logical Record Block containing the record.		    */
      rec_locate_block( han->slot, han->rid.bloc, 'U', &lrb );

/* Find the Chip Bitmap.						    */
      cbm = lrb;

/* Obtain a pointer to the Control Domain and unpack the Record Control	    */
/* Label.								    */
      cdom = lrb + REC__SZCBM + ( han->rid.chip * REC__SZCHIP );
      rec1_unpack_rcl( cdom, &rcl );

/* Derive a pointer to the Dynamic Domain and calculate the new length of   */
/* this domain.								    */
      ddom = cdom + REC__SZRCL + rcl.slen;
      dlen = rcl.dlen - extent;

/* If the Dynamic Domain is chained, then unpack the starting block number  */
/* of the associated frame of Pure Data Blocks.				    */
      if ( rcl.chain )
      {
         rec1_unpack_chain( ddom, &bloc );

/* Release any spare blocks resulting from the shrinkage.		    */
         spare = _nblocs( rcl.dlen ) - _nblocs( dlen );
         bloc += _nblocs( dlen );
         if ( ( spare > 0 ) && _ok( hds_gl_status ) )
         {
            rec1_deall_frame( han->slot, spare, bloc );
         }

/* Note if the record still needs to be chained.			    */
         rcl.chain = ( dlen > 0 );
      }

/* If the Dynamic Domain is not chained, then fill the spare chips in the   */
/* Logical Record Block with zeros.					    */
      else
      {
	 (void) memset( (void *) ( ddom + dlen ), 0, (size_t) extent );

/* Calculate the new record size and clear the appropriate bits in the Chip */
/* Bitmap.								    */
         size = _nchips( rcl.slen + _max( dlen, REC__SZCHAIN ) );
         spare = rcl.size - size;
         chip = han->rid.chip + size;
         rec1_clear_cbm( cbm, spare, chip );
         rcl.size = size;

/* Update the free space stack.						    */
         rec1_update_free( han->slot, han->rid.bloc, cbm );
      }

/* Reset the Dynamic Domain length and re-pack the Record Control Label.    */
      rcl.dlen = dlen;
      rec1_pack_rcl( &rcl, cdom );

/* Release the Logical Record Block.					    */
      if ( lrb != NULL ) rec_release_block( han->slot, han->rid.bloc );
   
/* Return the current global status value.				    */
      return hds_gl_status;
   }
