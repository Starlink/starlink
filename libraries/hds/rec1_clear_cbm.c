#include "hds1_feature.h"	 /* Define feature-test macros, etc.	    */
#include "hds1.h"		 /* Global definitions for HDS		    */
#include "rec1.h"		 /* Internal rec_ definitions		    */
#include "dat_err.h"		 /* DAT__ error code definitions	    */

   void rec1_clear_cbm( unsigned char cbm[ REC__SZCBM ], int nchip, int pos )
   {
/*+									    */
/* Name:								    */
/*    rec1_clear_cbm							    */

/* Purpose:								    */
/*    Clear bits in a chip bitmap.					    */

/* Invocation:								    */
/*    rec1_clear_cbm( cbm, nchip, pos )					    */

/* Description:								    */
/*    This function clears a specified number of bits in a chip bitmap to   */
/*    indicate that the associated Logical Record Block chips are free.	    */

/* Parameters:								    */
/*    unsigned char cbm[ REC__SZCBM ]					    */
/*	 The (packed) chip bitmap (i.e. exactly as contained in the Logical */
/*	 Record Block). The bitmap is modified by this routine.		    */
/*    int nchip								    */
/*       Number of consecutive chip entries to be cleared.		    */
/*    int pos								    */
/*	 Position of the first chip entry to be cleared (the first possible */
/*	 chip position is zero).					    */

/* Returned Value:							    */
/*    void								    */

/* Notes:								    */
/*    The implementation of this routine assumes that REC__SZCBM is equal   */
/*    to 2.								    */

/* Authors:								    */
/*    RFWS: R.F. Warren-Smith (STARLINK, RAL)				    */
/*    {@enter_new_authors_here@}					    */

/* History:								    */
/*    8-FEB-1991 (RFWS):						    */
/*       Original version.						    */
/*    25-MAR-1991 (RFWS):						    */
/*	 Changed to use unsigned quantities internally.			    */
/*    20-JUN-1991 (RFWS):						    */
/*       Added an initial status check.					    */
/*    {@enter_further_changes_here@}					    */

/* Bugs:								    */
/*    {@note_any_bugs_here@}						    */

/*-									    */

/* Local Variables:							    */
      unsigned int mask;	 /* Mask for clearing bits		    */
      unsigned int unpack;	 /* Un-packed chip bitmap value		    */

/*.									    */

/* Check the inherited global status.					    */
      if ( !_ok( hds_gl_status ) ) return;

/* Unpack the chip bitmap from its Logical Record Block format (this may    */
/* effect a byte order swap on some machines).				    */
      unpack = ( cbm[ 1 ] << 8 ) | cbm[ 0 ];

/* Set up a mask with nchip bits set at its low end.			    */
      mask = ~( ~0 << nchip );

/* If pos is zero, use this mask to clear the required number of bits	    */
/* directly. Otherwise, shift the mask to the required starting position    */
/* and then use it.							    */
      unpack = unpack & ~( pos == 0 ? mask : ( mask << pos ) );

/* Re-pack the new chip bitmap value.					    */
      cbm[ 0 ] = unpack & 0xff;
      cbm[ 1 ] = ( unpack >> 8 ) & 0xff;

/* Exit the routine.							    */
      return;
   }
