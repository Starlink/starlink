#if HAVE_CONFIG_H
#  include <config.h>
#endif

#include "hds1.h"
#include "rec1.h"

   void rec1_set_cbm( unsigned char cbm[ 2 ], int nchip, int pos )
   {
/*+                                                                         */
/* Name:                                                                    */
/*    rec1_set_cbm                                                          */

/* Purpose:                                                                 */
/*    Set bits in a chip bitmap.                                            */

/* Invocation:                                                              */
/*    rec1_set_cbm( cbm, nchip, pos )                                       */

/* Description:                                                             */
/*    This function sets a specified number of bits in a chip bitmap to     */
/*    indicate that the associated Logical Record Block chips are no longer */
/*    free.                                                                 */

/* Parameters:                                                              */
/*    unsigned char cbm[ 2 ]                                                */
/*       The (packed) chip bitmap (i.e. exactly as contained in the Logical */
/*       Record Block). The bitmap is modified by this routine.             */
/*    int nchip                                                             */
/*       Number of consecutive chip entries to be set.                      */
/*    int pos                                                               */
/*       Position of the first chip entry to be set (the first possible     */
/*       chip position is zero).                                            */

/* Returned Value:                                                          */
/*    void                                                                  */

/* Notes:                                                                   */
/*    This routine executes regardless of the HDS global status value.      */

/* Authors:                                                                 */
/*    RFWS: R.F. Warren-Smith (STARLINK, RAL)                               */
/*    {@enter_new_authors_here@}                                            */

/* History:                                                                 */
/*    8-FEB-1991 (RFWS):                                                    */
/*       Original version.                                                  */
/*    {@enter_changes_here@}                                                */

/* Bugs:                                                                    */
/*    {@note_any_bugs_here@}                                                */

/*-                                                                         */

/* Local Variables:                                                         */
      unsigned int mask;         /* Mask for setting bits                   */
      unsigned int unpack;       /* Un-packed chip bitmap value             */

/*.                                                                         */

/* Unpack the chip bitmap from its Logical Record Block format (this may    */
/* effect a byte order swap on some machines).                              */
      unpack = ( cbm[ 1 ] << 8 ) | cbm[ 0 ];

/* Set up a mask with nchip bits set at its low end.                        */
      mask = ~( ~0 << nchip );

/* If pos is zero, then use this mask to set the required number of bits    */
/* directly.  Otherwise, shift the mask to the required starting position   */
/* and then use it.                                                         */
      unpack = unpack | ( pos == 0 ? mask : ( mask << pos ) );

/* Re-pack the new chip bitmap value.                                       */
      cbm[ 0 ] = unpack & 0xff;
      cbm[ 1 ] = ( unpack >> 8 ) & 0xff;

/* Exit the routine.                                                        */
      return;
   }
