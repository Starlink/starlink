#if HAVE_CONFIG_H
#  include <config.h>
#endif

#include "dat_err.h"             /* DAT__ error code definitions            */

#include "hds1.h"                /* Global definitions for HDS              */
#include "rec1.h"

   int rec1_unpack_ncomp( const unsigned char pncomp[ 4 ], int *ncomp )
   {
/*+                                                                         */
/* Name:                                                                    */
/*    rec1_unpack_ncomp                                                     */

/* Purpose:                                                                 */
/*    Unpack number of components information from a Component Record.      */

/* Invocation:                                                              */
/*    rec1_unpack_ncomp( pncomp, ncomp )                                    */
/*                                                                          */
/* Description:                                                             */
/*    This function unpacks the "number of components" information stored   */
/*    in the Static Domain of a Structure Record. This is done so that the  */
/*    Static Domain format need not depend on the details of the way that   */
/*    this information is stored in memory.                                 */

/* Parameters:                                                              */
/*    const unsigned char pncomp[ 4 ]                                       */
/*       Pointer to an array of 4 Static Domain bytes to be unpacked.       */
/*    int *ncomp                                                            */
/*       Pointer to an integer to receive the number of structure           */
/*       components.                                                        */

/* Returned Value:                                                          */
/*    int rec1_unpack_ncomp                                                 */
/*       The global status value current on exit.                           */

/* Authors:                                                                 */
/*    RFWS: R.F. Warren-Smith (STARLINK)                                    */
/*    {@enter_new_authors_here@}                                            */

/* History:                                                                 */
/*    10-APR-1991 (RFWS):                                                   */
/*       Original version.                                                  */
/*    {@enter_changes_here@}                                                */

/* Bugs:                                                                    */
/*    {@note_any_bugs_here@}                                                */

/*-                                                                         */

/*.                                                                         */

/* Check the inherited global status.                                       */
      if ( !_ok( hds_gl_status ) ) return hds_gl_status;

/* Extract the number of components from the 4 bytes of the Static Domain.  */
      *ncomp = ( ( ( ( ( pncomp[ 3 ] << 8 ) |
                         pncomp[ 2 ] ) << 8 ) |
                         pncomp[ 1 ] ) << 8 ) |
                         pncomp[ 0 ];

/* Return the current global status value.                                  */
      return hds_gl_status;
   }
