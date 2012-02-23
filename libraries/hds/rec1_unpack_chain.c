#if HAVE_CONFIG_H
#  include <config.h>
#endif

#include "dat_err.h"             /* DAT__ error code definitions            */
#include "hds1.h"                /* Global definitions for HDS              */
#include "rec1.h"

   int rec1_unpack_chain( const unsigned char pchain[], int extended,
                          INT_BIG *chain )
   {
/*+                                                                         */
/* Name:                                                                    */
/*    rec1_unpack_chain                                                     */

/* Purpose:                                                                 */
/*    Unpack the starting block number from a chained record.               */

/* Invocation:                                                              */
/*    rec1_unpack_chain( pchain, extended, chain )                          */
/*                                                                          */
/* Description:                                                             */
/*    This function unpacks the number of the Pure Data Block in which a    */
/*    record's data begins from the Dynamic Domain of a "chained" record.   */

/* Parameters:                                                              */
/*    const unsigned char pchain[]                                          */
/*       Pointer to an array of wither 4 (HDS V3) or 8 (HDS V4)  Dynamic    */
/*       Domain bytes to be unpacked.                                       */
/*    int extended                                                          */
/*       Flag HDS3 = 0 HDS4 = 1                                             */
/*    INT_BIG *chain                                                        */
/*       Pointer to an integer to receive the block number of the first     */
/*       (chained) Pure Data Block.                                         */

/* Returned Value:                                                          */
/*    int rec1_unpack_chain                                                 */
/*       The global status value current on exit.                           */

/* Authors:                                                                 */
/*    RFWS: R.F. Warren-Smith (STARLINK)                                    */
/*    BKM:  B.K. McIlwrath    (STARLINK)                                    */
/*    {@enter_new_authors_here@}                                            */

/* History:                                                                 */
/*    15-APR-1991 (RFWS):                                                   */
/*       Original version.                                                  */
/*    19-JUN-2002 (BKM):                                                    */
/*       Revise for extended format (64-bit) HDS files.                     */
/*    {@enter_changes_here@}                                                */

/* Bugs:                                                                    */
/*    {@note_any_bugs_here@}                                                */

/*-                                                                         */

/*.                                                                         */

/* Check the inherited global status.                                       */
      if ( !_ok( hds_gl_status ) ) return hds_gl_status;

/* Extract the block number from the 4/8 bytes of the Dynamic Domain.       */
      if( extended )
         *chain = ( ( ( ( ( ( ( ( ( ( ( ( (
                           pchain[ 7 ] << 8 ) |
                           pchain[ 6 ] ) << 8 ) |
                           pchain[ 5 ] ) << 8 ) |
                           pchain[ 4 ] ) << 8 ) |
                           pchain[ 3 ] ) << 8 ) |
                           pchain[ 2 ] ) << 8 ) |
                           pchain[ 1 ] ) << 8 ) |
                           pchain[ 0 ];
      else {
         *chain = 0;
         *chain = ( ( ( ( ( pchain[ 3 ] << 8 ) |
                            pchain[ 2 ] ) << 8 ) |
                            pchain[ 1 ] ) << 8 ) |
		            pchain[ 0 ];
      }

/* Return the current global status value.                                  */
      return hds_gl_status;
   }
