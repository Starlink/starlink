#include "hds1_feature.h"	 /* Define feature-test macros, etc.	    */
#include "dat_err.h"		 /* DAT__ error code definitions	    */
#include "hds1.h"		 /* Global definitions for HDS		    */

   int rec1_pack_chain( int chain, unsigned char pchain[ 4 ] )
   {
/*+									    */
/* Name:								    */
/*    rec1_pack_chain							    */

/* Purpose:								    */
/*    Pack the starting block number for a chained record.		    */

/* Invocation:								    */
/*    rec1_pack_chain( chain, pchain )					    */
/*									    */
/* Description:								    */
/*    This function packs the number of the Pure Data Block in which a	    */
/*    record's data begins into the Dynamic Domain of a "chained" record.   */

/* Parameters:								    */
/*    int chain								    */
/*	 Block number of the first Pure Data Block (the first block in the  */
/*	 file is no. 1).						    */
/*    unsigned char pchain[ 4 ]						    */
/*	 Pointer to an array of 4 Dynamic Domain bytes into which the	    */
/*	 information is to be packed.					    */

/* Returned Value:							    */
/*    int rec1_pack_chain						    */
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

/* Pack the starting block number into the 4 bytes of the Static Domain.    */
      pchain[ 0 ] = chain & 0xff;
      pchain[ 1 ] = ( chain >> 8 ) & 0xff;
      pchain[ 2 ] = ( chain >> 16 ) & 0xff;
      pchain[ 3 ] = ( chain >> 24 ) & 0xff;

/* Return the current global status value.				    */
      return hds_gl_status;
   }
