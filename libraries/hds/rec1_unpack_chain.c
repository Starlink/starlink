#include "hds1_feature.h"	 /* Define feature-test macros, etc.	    */
#include "dat_err.h"		 /* DAT__ error code definitions	    */
#include "hds1.h"		 /* Global definitions for HDS		    */

   int rec1_unpack_chain( const unsigned char pchain[ 4 ], int *chain )
   {
/*+									    */
/* Name:								    */
/*    rec1_unpack_chain							    */

/* Purpose:								    */
/*    Unpack the starting block number from a chained record.		    */

/* Invocation:								    */
/*    rec1_unpack_chain( pchain, chain )				    */
/*									    */
/* Description:								    */
/*    This function unpacks the number of the Pure Data Block in which a    */
/*    record's data begins from the Dynamic Domain of a "chained" record.   */

/* Parameters:								    */
/*    const unsigned char pchain[ 4 ]					    */
/*	 Pointer to an array of 4 Dynamic Domain bytes to be unpacked.	    */
/*    int *chain							    */
/*	 Pointer to an integer to receive the block number of the first	    */
/*	 (chained) Pure Data Block.					    */

/* Returned Value:							    */
/*    int rec1_unpack_chain						    */
/*       The global status value current on exit.			    */

/* Authors:								    */
/*    RFWS: R.F. Warren-Smith (STARLINK)				    */
/*    {@enter_new_authors_here@}					    */

/* History:								    */
/*    15-APR-1991 (RFWS):						    */
/*       Original version.						    */
/*    {@enter_changes_here@}						    */

/* Bugs:								    */
/*    {@note_any_bugs_here@}						    */

/*-									    */

/*.									    */

/* Check the inherited global status.					    */
      if ( !_ok( hds_gl_status ) ) return hds_gl_status;

/* Extract the starting block number from the 4 bytes of the Static Domain. */
      *chain = ( ( ( ( ( pchain[ 3 ] << 8 ) |
                         pchain[ 2 ] ) << 8 ) |
                         pchain[ 1 ] ) << 8 ) |
			 pchain[ 0 ];

/* Return the current global status value.				    */
      return hds_gl_status;
   }
