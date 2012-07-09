#if HAVE_CONFIG_H
#  include <config.h>
#endif

#include "hds1.h"
#include "rec1.h"

   int rec1_test_cbm( const unsigned char cbm[ 2 ], int start, int nchip )
   {
/*+                                                                         */
/* Name:                                                                    */
/*    rec1_test_cbm                                                         */

/* Purpose:                                                                 */
/*    Test if a sequence of chips are free in a chip bitmap.                */

/* Invocation:                                                              */
/*    rec1_test_cbm( cbm, start, nchip )                                    */

/* Description:                                                             */
/*    This function tests whether a specific sequence of chips are free in  */
/*    a Logical Record Block by testing whether the associated bits in its  */
/*    chip bitmap are clear.                                                */

/* Parameters:                                                              */
/*    const unsigned char cbm[ 2 ]                                                  */
/*       The (packed) chip bitmap (i.e. exactly as contained in the Logical */
/*       Record Block).                                                     */
/*    int start                                                             */
/*       Number of the first chip to be tested (the first possible chip is  */
/*       no. zero).                                                         */
/*    int nchip                                                             */
/*       Number of consecutive chips to be tested.                          */

/* Returned Value:                                                          */
/*    int rec1_test_cbm                                                     */
/*       Returns 1 if all the requested chips are free, otherwise 0.        */

/* Authors:                                                                 */
/*    RFWS: R.F. Warren-Smith (STARLINK, RAL)                               */
/*    {@enter_new_authors_here@}                                            */

/* History:                                                                 */
/*    5-APR-1991 (RFWS):                                                    */
/*       Original version.                                                  */
/*    {@enter_changes_here@}                                                */

/* Bugs:                                                                    */
/*    {@note_any_bugs_here@}                                                */

/*-                                                                         */

/* Local Variables:                                                         */
      unsigned int mask;         /* Mask for clearing bits                  */
      unsigned int unpack;       /* Un-packed chip bitmap value             */

/*.                                                                         */

/* Unpack the chip bitmap from its Logical Record Block format (this may    */
/* effect a byte order swap on some machines).                              */
      unpack = ( cbm[ 1 ] << 8 ) | cbm[ 0 ];

/* Set up a mask with nchip bits set at its low end.                        */
      mask = ~( ~0 << nchip );

/* If start is zero, use this mask to test for the required number of clear */
/* bits directly. Otherwise, shift the mask to the required starting        */
/* position and then use it.                                                */
      return ( 0 == ( unpack & ( start == 0 ? mask : ( mask << start ) ) ) );
   }
