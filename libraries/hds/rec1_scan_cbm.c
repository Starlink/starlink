#if HAVE_CONFIG_H
#  include <config.h>
#endif

#include "hds1.h"                /* Global definitions for HDS              */
#include "rec1.h"                /* Internal rec_ definitions               */

   int rec1_scan_cbm( const unsigned char cbm[ 2 ], int nchip, int *pos )
   {
/*+                                                                         */
/* Name:                                                                    */
/*    rec1_scan_cbm                                                         */

/* Purpose:                                                                 */
/*    Scan a chip bitmap for a sequence of free chips.                      */

/* Invocation:                                                              */
/*    rec1_scan_cbm( cbm, nchip, pos )                                      */

/* Description:                                                             */
/*    This function scans a chip bitmap which describes the free space      */
/*    available in a Logical Record Block, and returns the position of the  */
/*    first available sequence of free chips of a given size (a free chip   */
/*    is indicated by a clear bit in the bitmap).                           */

/* Parameters:                                                              */
/*    const unsigned char cbm[ 2 ]                                          */
/*       The (packed) chip bitmap (i.e. exactly as contained in the Logical */
/*       Record Block).                                                     */
/*    int nchip                                                             */
/*       Number of consecutive chips required.                              */
/*    int *pos                                                              */
/*       Pointer to an integer in which the position of the first chip in   */
/*       the sequence will be returned (the first possible chip position is */
/*       zero). If no suitable sequence of free chips is found, then this   */
/*       integer will remain unchanged.                                     */

/* Returned Value:                                                          */
/*    int rec1_scan_cbm                                                     */
/*       1 if a suitable sequence of free chips was found, otherwise 0.     */

/* Notes:                                                                   */
/*    This routine executes regardless of the HDS global status value.      */

/* Authors:                                                                 */
/*    RFWS: R.F. Warren-Smith (STARLINK, RAL)                               */
/*    {@enter_new_authors_here@}                                            */

/* History:                                                                 */
/*    8-FEB-1991 (RFWS):                                                    */
/*       Original version.                                                  */
/*    25-MAR-1991 (RFWS):                                                   */
/*       Changed to use unsigned quantities internally and fixed bug in     */
/*       size of chip bitmap.                                               */
/*    2-APR-1991 (RFWS):                                                    */
/*       Changed so that *pos is not changed unless the required sequence   */
/*       of free chips is found.                                            */
/*    {@enter_changes_here@}                                                */

/* Bugs:                                                                    */
/*    {@note_any_bugs_here@}                                                */

/*-                                                                         */

/* Local Variables:                                                         */
      int found;                 /* Whether a sequence was found            */
      int i;                     /* Loop counter for searching bitmap       */
      unsigned int mask;         /* Search mask                             */
      unsigned int unpack;       /* Un-packed chip bitmap value             */

/*.                                                                         */

/* Unpack the chip bitmap from its Logical Record Block format (this may    */
/* effect a byte reversal on some machines).                                */
      unpack = ( cbm[ 1 ] << 8 ) | cbm[ 0 ];

/* Set up a search mask with nchip bits set at its low end.                 */
      mask = ~( ~0 << nchip );

/* Search through the unpacked bitmap for a sequence of free chips.         */
      found = 0;
      for ( i = 0; i < ( REC__MXCHIP + 1 - nchip ); i++ )
      {

/* Quit if found.                                                           */
         if ( ( unpack & mask ) == 0 )
         {
            found = 1;
            break;
         }

/* Shift the mask to search each possible position.                         */
         mask = ( mask << 1 );
      }

/* If a sequence of free chips was found, then return its position.         */
      if ( found ) *pos = i;

/* Return whether found                                                     */
      return found;
   }
